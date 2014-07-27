{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Tightrope as TR
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

emojis :: [TR.Icon]
emojis = fmap TR.Icon ["rat", "mouse2", "ox", "water_buffalo", "cow2", "tiger2", "leopard", "rabbit2", "cat2", "dragon", "crocodile", "whale2", "snail", "snake", "racehorse", "ram", "goat", "sheep", "monkey", "rooster", "chicken", "dog2", "pig2", "boar", "elephant", "octopus", "shell", "bug", "ant", "honeybee", "beetle", "fish", "tropical_fish", "blowfish", "turtle", "hatching_chick", "baby_chick", "hatched_chick", "bird", "penguin", "koala", "poodle", "dromedary_camel", "camel", "flipper", "mouse", "cow", "tiger", "rabbit", "cat", "dragon_face", "whale", "horse", "monkey_face", "dog", "pig", "frog", "hamster", "wolf", "bear", "panda_face"]

randomEmoji :: IO TR.Icon
randomEmoji = do
  i <- getStdRandom $ randomR (0, length emojis - 1)
  return (emojis !! i) -- yolo

srcParser :: Parser Text
srcParser = manyTill anyChar (asciiCI "src=\"") *> takeTill (== '"')

jpgUrl :: [Text] -> IO (Either String Text)
jpgUrl inputWords = do
  res <- Wreq.get (unpack url)
  return $ case parseLazy srcParser (res ^. Wreq.responseBody) of
    Left e -> Left $ mconcat ["Error parsing <img> tag: ", e]
    Right imageUrl -> Right imageUrl
  where parseLazy p = parseOnly p . decodeUtf8 . toStrict
        hostname = intercalate "." inputWords
        url = mconcat ["http://", hostname, ".jpg.to"]

handler :: TR.Command -> TR.Slack Text
handler command = do
  icon <- TR.liftIO randomEmoji
  case words (command ^. TR.text) of
    [] -> return "You need to ask for some words!"
    wordsRequested -> do
      parseResult <- TR.liftIO $ jpgUrl wordsRequested
      case parseResult of
        Left errorString -> return (pack errorString)
        Right url -> do
          let message = TR.message icon "jpg2bot" (mconcat [unwords wordsRequested, ": ", url])
          TR.say message (command ^. TR.source)
          return ""

main :: IO ()
main = do
  token <- readFile "token"
  let url = "https://trello.slack.com/services/hooks/incoming-webhook"
      account = TR.Account token url
      port = 4000
  putStrLn $ "Running on port " ++ show port
  Warp.run port $ TR.bot account handler
