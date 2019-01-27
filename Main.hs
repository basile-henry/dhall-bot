{-# Language OverloadedStrings #-}

module Main where

-- base
import           Control.Arrow
import           Control.Exception
import           Data.List
import           System.Exit
import           System.IO

-- network-simple
import           Network.Simple.TCP   (Socket)
import qualified Network.Simple.TCP   as Network

-- mtl
import           Control.Monad.Reader

-- text
import           Data.Text            as Text
import           Data.Text            (Text)
import           Data.Text.Encoding   as Text
import           Data.Text.IO         as Text

chan   = "#dhall-test"
nick   = "dhall-bot"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Socket IO

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = Network.connect "irc.freenode.org" "6667" $ runReaderT run . fst

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
        eval param

-- Dispatch a command
eval :: Text -> Net ()
eval x
  | "!id " `Text.isPrefixOf` x
  = privmsg (Text.drop 4 x)
eval _ = pure () -- ignore everything else

-- Send a privmsg to the current chan + server
privmsg :: Text -> Net ()
privmsg msg = write "PRIVMSG" (chan <> " :" <> msg)

-- Send a message out to the server we're currently connected to
write :: Text -> Text -> Net ()
write command params = do
  let ircLine = command <> " " <> params <> "\r\n"
  socket <- ask
  liftIO $ do
    Network.send socket (Text.encodeUtf8 ircLine)
    Text.putStr ircLine
