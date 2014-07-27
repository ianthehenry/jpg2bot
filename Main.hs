{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Tightrope as TR
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.ByteString.Lazy (toStrict)
import           Control.Lens ((^.))
import           System.Random (getStdRandom, randomR)
import qualified Network.Wreq as Wreq
import           Prelude hiding (words, unwords)
import           Data.Attoparsec.Text
import           Control.Applicative
import           Data.Monoid (mconcat, mappend)
import           Control.Arrow (left)
import           Data.Char (isAscii)

randomEmoji :: IO TR.Icon
randomEmoji = (emojis !!) <$> getStdRandom (randomR (0, length emojis - 1))
  where emojis = fmap TR.Icon ["rat", "mouse2", "ox", "water_buffalo", "cow2", "tiger2", "leopard", "rabbit2", "cat2", "dragon", "crocodile", "whale2", "snail", "snake", "racehorse", "ram", "goat", "sheep", "monkey", "rooster", "chicken", "dog2", "pig2", "boar", "elephant", "octopus", "shell", "bug", "ant", "honeybee", "beetle", "fish", "tropical_fish", "blowfish", "turtle", "hatching_chick", "baby_chick", "hatched_chick", "bird", "penguin", "koala", "poodle", "dromedary_camel", "camel", "flipper", "mouse", "cow", "tiger", "rabbit", "cat", "dragon_face", "whale", "horse", "monkey_face", "dog", "pig", "frog", "hamster", "wolf", "bear", "panda_face"]

jpgUrl :: [Text] -> IO (Either String Text)
jpgUrl inputWords = (left prettifyError . parseLazy . (^. Wreq.responseBody)) <$> Wreq.get url
  where prettifyError = mappend "Error parsing <img> tag: "
        parseLazy = parseOnly p . decodeUtf8 . toStrict
        p = manyTill anyChar (asciiCI "src=\"") *> takeTill (== '"')
        hostname = T.intercalate "." inputWords
        url = T.unpack $ mconcat ["http://", hostname, ".jpg.to"]

handler :: TR.Command -> TR.Slack Text
handler command = do
  icon <- TR.liftIO randomEmoji
  case (T.words . T.filter isAscii) (command ^. TR.text) of
    [] -> return "You need to ask for some words!"
    wordsRequested -> TR.liftIO (jpgUrl wordsRequested) >>= either (return . T.pack) (report $>> "")
      where (f $>> result) arg = f arg >> return result
            report url = TR.say (message url) (command ^. TR.source)
            message url = TR.message icon "jpg2bot" (mconcat [T.unwords wordsRequested, ": ", url])

main :: IO ()
main = do
  token <- readFile "token"
  let url = "https://trello.slack.com/services/hooks/incoming-webhook"
      account = TR.Account token url
      port = 4000
  putStrLn $ "Running on port " ++ show port
  Warp.run port $ TR.bot account handler
