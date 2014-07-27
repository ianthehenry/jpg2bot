{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Network.Tightrope as Tightrope
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Tightrope (Slack, say, message, text, source, user, name, username, iconEmoji, destination, liftIO)
import           Data.Text (Text, words, unwords, intercalate, unpack, pack)
import           Data.Text.Encoding (decodeUtf8)
import           Data.ByteString.Lazy (toStrict)
import           Control.Lens
import           System.Random (getStdRandom, randomR)
import qualified Network.Wreq as Wreq
import           Prelude hiding (words, unwords)
import           Data.Attoparsec.Text
import           Control.Applicative
import           Data.Monoid (mconcat)

emojis :: [Tightrope.Icon]
emojis = fmap Tightrope.Icon ["rat", "mouse2", "ox", "water_buffalo", "cow2", "tiger2", "leopard", "rabbit2", "cat2", "dragon", "crocodile", "whale2", "snail", "snake", "racehorse", "ram", "goat", "sheep", "monkey", "rooster", "chicken", "dog2", "pig2", "boar", "elephant", "octopus", "shell", "bug", "ant", "honeybee", "beetle", "fish", "tropical_fish", "blowfish", "turtle", "hatching_chick", "baby_chick", "hatched_chick", "bird", "penguin", "koala", "poodle", "dromedary_camel", "camel", "flipper", "mouse", "cow", "tiger", "rabbit", "cat", "dragon_face", "whale", "horse", "monkey_face", "dog", "pig", "frog", "hamster", "wolf", "bear", "panda_face"]

randomEmoji :: IO Tightrope.Icon
randomEmoji = do
  i <- getStdRandom $ randomR (0, length emojis - 1)
  return (emojis !! i) -- yolo

srcParser :: Parser Text
srcParser = manyTill anyChar (asciiCI "src=\"") *> takeTill (== '"')

jpgUrl :: [Text] -> IO Text
jpgUrl inputWords = do
  res <- Wreq.get (unpack url)
  return $ case parseLazy srcParser (res ^. Wreq.responseBody) of
    Left e -> mconcat ["Error parsing <img> tag: ", pack e]
    Right imageUrl -> imageUrl
  where parseLazy p = parseOnly p . decodeUtf8 . toStrict
        hostname = intercalate "." inputWords
        url = mconcat ["http://", hostname, ".jpg.to"]

handler :: Tightrope.Command -> Slack Text
handler command = do
  icon <- liftIO randomEmoji
  case words (command ^. text) of
    [] -> return "You need to ask for some words!"
    wordsRequested -> do
      url <- liftIO $ jpgUrl wordsRequested
      say $ message icon
                    "jpg2bot"
                    (mconcat [unwords wordsRequested, ": ", url])
                    (command ^. source)
      return ""

main :: IO ()
main = do
  token <- readFile "token"
  let url = "https://trello.slack.com/services/hooks/incoming-webhook"
      account = Tightrope.Account token url
      port = 4000
  putStrLn $ "Running on port " ++ show port
  Warp.run port $ Tightrope.bot account handler
