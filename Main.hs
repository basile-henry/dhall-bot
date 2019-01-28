{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- base
import           Control.Exception
import           Data.Bifunctor
import           Data.List
import           System.Exit
import           System.IO

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
import           Control.Monad.Except
import           Control.Monad.Reader

-- prettyprinter
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty

-- text
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import qualified Data.Text.Encoding                    as Text
import qualified Data.Text.IO                          as Text

-- transformers
import qualified Control.Monad.Trans.State.Strict      as State


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
    mRawLine <- liftIO (Network.recv socket 512) -- IRC lines limited to 512 bytes
    case mRawLine of
      Just rawLine -> do
        let ircLine =  Text.decodeUtf8 rawLine
            command:params = Text.splitOn " :" ircLine
            param = Text.concat params

        if "PING " == command
          then write "PONG" $ " :" <> param
          else do
            liftIO $ Text.putStr ircLine
            evalIrc param
      Nothing -> pure ()

-- Dispatch a command
evalIrc
  :: ( MonadIO m, MonadReader Socket m )
  => Text -> m ()
evalIrc x
  -- eval dhall
  | ">" `Text.isPrefixOf` x
  = privmsg =<< eval False (Text.drop 1 x)

  -- type dhall
  |  ":type " `Text.isPrefixOf` x
  = privmsg =<< eval True (Text.drop 6 x)
  | ":t " `Text.isPrefixOf` x
  = privmsg =<< eval True (Text.drop 3 x)

evalIrc _ = pure () -- ignore everything else

-- Send a privmsg to the current chan + server
privmsg
  :: ( MonadIO m, MonadReader Socket m )
  => Text -> m ()
privmsg msg = mapM_ (\m -> write "PRIVMSG" (chan <> " :" <> m)) $ Text.lines msg

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

eval
  :: forall m . MonadIO m
  => Bool -> Text -> m Text
eval showType src = fmap (either id id) . runExceptT $ do
  let withErr :: Show e => Either e a -> ExceptT Text m a
      withErr = liftEither . first (Text.pack . show)

  parsed <-
    withErr $ Dhall.exprFromText "(stdin)" src

  loaded <- withErr =<< liftIO
    (catch
      (Right <$> State.evalStateT (Dhall.loadWith parsed) (Dhall.emptyStatus "."))
      (\(e :: Dhall.MissingImports) -> pure $ Left e))

  type' <-
    withErr $ Dhall.typeOf loaded

  pure . output . Dhall.normalize $ if showType then type' else loaded

output :: Pretty.Pretty a => Dhall.Expr s a -> Text
output expr =
  let stream =
        Pretty.unAnnotateS $
          Pretty.layoutSmart Dhall.Pretty.layoutOpts $
              Dhall.Pretty.prettyCharacterSet Dhall.Pretty.Unicode expr

  in  Pretty.renderStrict stream
