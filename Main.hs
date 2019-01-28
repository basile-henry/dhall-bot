{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

-- base
import           Control.Arrow
import           Control.Exception
import           Data.List
import           System.Exit
import           System.IO

-- prettyprinter
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty

-- dhall
import qualified Dhall
import qualified Dhall.Binary
import qualified Dhall.Context
import qualified Dhall.Core
import qualified Dhall.Core                            as Dhall (Expr, Var (V),
                                                                 normalize)
import qualified Dhall.Core                            as Expr (Expr (..))
import qualified Dhall.Import                          as Dhall
import qualified Dhall.Map                             as Map
import qualified Dhall.Parser                          as Dhall
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck                       as Dhall

-- network-simple
import           Network.Simple.TCP                    (Socket)
import qualified Network.Simple.TCP                    as Network

-- mtl
import           Control.Monad.Reader
import qualified Control.Monad.Trans.State.Strict      as State

-- text
import qualified Data.Text                             as Text
import           Data.Text                             (Text)
import qualified Data.Text.Encoding                    as Text
import qualified Data.Text.IO                          as Text

server = "irc.freenode.org"
port   = "6667"
chan   = "#dhall-test"
nick   = "dhall-bot"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Socket IO

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = Network.connect server port $ runReaderT run . fst

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
  write "NICK" nick
  write "USER" (nick <> " 0 * :Dhall bot")
  write "JOIN" chan
  listen

-- Process each line from the server
listen :: Net ()
listen = do
  socket <- ask
  forever $ do
    Just rawLine <- liftIO (Network.recv socket 512) -- IRC lines limited to 512 bytes
    let ircLine =  Text.decodeUtf8 rawLine
        command:params = Text.splitOn " :" ircLine
        param = Text.concat params

    if "PING " == command
      then write "PONG" $ " :" <> param
      else do
        liftIO $ Text.putStr ircLine
        evalIrc param

-- Dispatch a command
evalIrc :: Text -> Net ()
evalIrc x
  | "!id " `Text.isPrefixOf` x
  = privmsg (Text.drop 4 x)
  | ">" `Text.isPrefixOf` x
  = eval $ Text.drop 1 x
evalIrc _ = pure () -- ignore everything else

-- Send a privmsg to the current chan + server
privmsg
  :: ( MonadIO m, MonadReader Socket m )
  => Text -> m ()
privmsg msg = write "PRIVMSG" (chan <> " :" <> msg)

-- Send a message out to the server we're currently connected to
write
  :: ( MonadIO m, MonadReader Socket m )
  => Text -> Text -> m ()
write command params = do
  let ircLine = command <> " " <> params <> "\r\n"
  socket <- ask
  liftIO $ do
    Network.send socket (Text.encodeUtf8 ircLine)
    Text.putStr ircLine

-----------
-- Dhall
-----------

parseAndLoad
  :: ( MonadIO m )
  => Text -> m ( Dhall.Expr Dhall.Src Dhall.X )
parseAndLoad src = do

  parsed <-
    case Dhall.exprFromText "(stdin)" src of
      Left e ->
        liftIO ( throwIO e )

      Right a ->
        return a

  let status = Dhall.emptyStatus "."

  liftIO ( State.evalStateT (Dhall.loadWith parsed) status )

typeCheck
  :: ( MonadIO m )
  => Dhall.Expr Dhall.Src Dhall.X -> m ( Dhall.Expr Dhall.Src Dhall.X )
typeCheck expression =
  case Dhall.typeOf expression of
    Left  e -> liftIO ( throwIO e )
    Right a -> return a

eval
  :: ( MonadIO m, MonadReader Socket m )
  => Text -> m ()
eval src = do
  loaded <-
    parseAndLoad src

  exprType <-
    typeCheck loaded

  let expr = Dhall.normalize loaded

  output expr

output
    :: (Pretty.Pretty a, MonadIO m, MonadReader Socket m)
    => Dhall.Expr s a -> m ()
output expr = do

  let stream =
        Pretty.unAnnotateS $
          Pretty.layoutSmart Dhall.Pretty.layoutOpts $
              Dhall.Pretty.prettyCharacterSet Dhall.Pretty.Unicode expr

  privmsg $ Pretty.renderStrict stream
