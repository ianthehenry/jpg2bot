{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Network.Tightrope as Tightrope
import qualified Network.Wai.Handler.Warp as Warp
import Network.Tightrope (Slack, text, source, user, name, username, iconEmoji, destination, liftIO)
import Data.Text (Text)
import Control.Lens
import System.Random (getStdRandom, randomR)

emojis :: [Tightrope.Icon]
emojis = fmap Tightrope.Icon ["rat", "mouse2", "ox", "water_buffalo", "cow2", "tiger2", "leopard", "rabbit2", "cat2", "dragon", "crocodile", "whale2", "snail", "snake", "racehorse", "ram", "goat", "sheep", "monkey", "rooster", "chicken", "dog2", "pig2", "boar", "elephant", "octopus", "shell", "bug", "ant", "honeybee", "beetle", "fish", "tropical_fish", "blowfish", "turtle", "hatching_chick", "baby_chick", "hatched_chick", "bird", "penguin", "koala", "poodle", "dromedary_camel", "camel", "flipper", "mouse", "cow", "tiger", "rabbit", "cat", "dragon_face", "whale", "horse", "monkey_face", "dog", "pig", "frog", "hamster", "wolf", "bear", "panda_face"]

randomEmoji :: IO Tightrope.Icon
randomEmoji = do
  i <- getStdRandom $ randomR (0, length emojis - 1)
  return (emojis !! i)

handler :: Tightrope.Command -> Slack Text
handler command = do
  icon <- liftIO randomEmoji
  let message = Tightrope.defaultMessage & text .~ "hello"
                                         & destination .~ (command ^. source)
                                         & iconEmoji .~ icon
                                         & username .~ "Tightrope bot"
  Tightrope.say message
  return (command ^. text)

main :: IO ()
main = do
  token <- readFile "token"
  let url = "https://trello.slack.com/services/hooks/incoming-webhook"
      account = Tightrope.Account token url
      port = 4000
  putStrLn $ "Running on port " ++ show port
  Warp.run port $ Tightrope.bot account handler
