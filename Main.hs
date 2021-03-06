{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

-- base
import           Control.Exception
import           Data.Bifunctor

-- dhall
import qualified Dhall.Core                            as Dhall
import qualified Dhall.Import                          as Dhall
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


server, port :: String
server = "irc.freenode.org"
port   = "6667"

channel, nick :: Text
channel = "#dhall-test"
nick    = "dhall-bot-test"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Socket IO

data Msg =
  Msg
    { chan    :: Text
    , content :: Text
    }

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = Network.connect server port $ runReaderT run . fst

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
  write $ "NICK " <> nick
  write $ "USER " <> nick <> " 0 * :Dhall bot"
  write $ "JOIN " <> channel
  listen

-- Process each line from the server
listen :: Net ()
listen = do
  socket <- ask
  forever $ do
    mRawLine <- liftIO (Network.recv socket 512) -- IRC lines limited to 512 bytes
    case mRawLine of
      Just rawLine -> do
        let ircLine = Text.decodeUtf8 rawLine

        case Text.words ircLine of
          -- Respond to ping messages to stay connected
          "PING":rest -> write $ Text.unwords ("PONG" : rest)

          -- Send the message either to the channel or as a private message
          who:"PRIVMSG":c:(Text.drop 1 . Text.unwords -> msgContent)
            -- Channel message
            | c == channel
            -> do
            -- For the logs
            liftIO $ Text.putStr ircLine

            evalIrc . Msg c $ msgContent

            -- Private message
            | c == nick
            , (w:_) <- Text.splitOn "!" $ Text.drop 1 who
            -> do
            -- For the logs
            liftIO $ Text.putStr ircLine

            evalIrc . Msg w $ msgContent


          _                    -> pure ()
      Nothing -> pure ()

-- Dispatch a command
evalIrc
  :: ( MonadIO m, MonadReader Socket m )
  => Msg -> m ()
evalIrc (Msg c x)
  -- eval dhall
  | ">" `Text.isPrefixOf` x
  = evalDhall False (Text.drop 1 x)

  -- type dhall
  |  ":type " `Text.isPrefixOf` x
  = evalDhall True (Text.drop 6 x)
  | ":t " `Text.isPrefixOf` x
  = evalDhall True (Text.drop 3 x)

  -- help
  |  ":help" `Text.isPrefixOf` x
  || ":h" `Text.isPrefixOf` x
  = msg "I understand the following commands: about help type >"

  -- help
  | ":about" `Text.isPrefixOf` x
  = msg "The source for the dhall-bot project is available at https://github.com/basile-henry/dhall-bot"
  where
    msg = privmsg . Msg c

    evalDhall t y = msg =<< eval t y

evalIrc _ = pure () -- ignore everything else

-- Send a privmsg to the current chan + server
privmsg
  :: ( MonadIO m, MonadReader Socket m )
  => Msg -> m ()
privmsg msg =
  let ls = Text.lines $ content msg
      limitedLines
        | length ls > 5 = take 4 ls ++ [ "..." ]
        | otherwise     = ls
  in  mapM_ (\m -> write $ "PRIVMSG " <> chan msg <> " :" <> m) limitedLines

-- Send a message out to the server we're currently connected to
write
  :: ( MonadIO m, MonadReader Socket m )
  => Text -> m ()
write command = do
  let ircLine = command <> "\r\n"
  socket <- ask
  liftIO $ do
    Network.send socket (Text.encodeUtf8 ircLine)

    unless ("PONG :" `Text.isPrefixOf` ircLine) $
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
