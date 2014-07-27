{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

import           BasePrelude hiding (words, unwords, intercalate, filter)
import           Control.Lens ((^.))
import           Data.Attoparsec.Text hiding (parse)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.Configurator as Conf
import           Data.Text (Text, pack, unpack, intercalate, words, unwords, filter)
import           Data.Text.Encoding (decodeUtf8)
import qualified Network.Tightrope as TR
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wreq as Wreq
import           System.Random (getStdRandom, randomR)

randomEmoji :: IO TR.Icon
randomEmoji = (emojis !!) <$> getStdRandom (randomR (0, length emojis - 1))
  where emojis = fmap TR.Icon ["rat", "mouse2", "ox", "water_buffalo", "cow2", "tiger2", "leopard", "rabbit2", "cat2", "dragon", "crocodile", "whale2", "snail", "snake", "racehorse", "ram", "goat", "sheep", "monkey", "rooster", "chicken", "dog2", "pig2", "boar", "elephant", "octopus", "shell", "bug", "ant", "honeybee", "beetle", "fish", "tropical_fish", "blowfish", "turtle", "hatching_chick", "baby_chick", "hatched_chick", "bird", "penguin", "koala", "poodle", "dromedary_camel", "camel", "flipper", "mouse", "cow", "tiger", "rabbit", "cat", "dragon_face", "whale", "horse", "monkey_face", "dog", "pig", "frog", "hamster", "wolf", "bear", "panda_face"]

jpgUrl :: [Text] -> IO (Either String Text)
jpgUrl inputWords = (parse . response) <$> Wreq.get url
  where response = decodeUtf8 . LBS.toStrict . (^. Wreq.responseBody)
        parse = parseOnly $ manyTill anyChar (asciiCI "src=\"") *> takeTill (== '"')
        url = unpack $ mconcat ["http://", intercalate "." inputWords, ".jpg.to"]

handler :: TR.Command -> TR.Slack Text
handler command = TR.liftIO randomEmoji >>= \icon ->
  case (words . filter isAscii) (command ^. TR.text) of
    [] -> return "You need to ask for some (ASCII) words!"
    wordsRequested -> TR.liftIO (jpgUrl wordsRequested) >>= either (return . pack) (report $>> "")
      where (f $>> result) arg = f arg >> return result
            report url = TR.say (message url) (command ^. TR.source)
            message url = TR.message icon "jpg2bot" (mconcat ["<", url, "|", unwords wordsRequested, ">"])

main :: IO ()
main = do
  account <- liftM2 TR.Account (conf "token") (conf "url")
  port <- conf "port"
  putStrLn ("Running on port " ++ show port)
  Warp.run port (TR.bot account handler)
  where conf key = Conf.load [Conf.Required "conf"] >>= flip Conf.require key
