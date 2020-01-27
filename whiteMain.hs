-- DO NOT SUBMIT THIS FILE

module Main where

import GameEngine

-- uncomment one of the following lines to select the Black player
import qualified RandomAI as Black
--import qualified ReversiAI as Black

-- uncomment one of the following lines to select the White player
--import qualified RandomAI as White
import qualified ReversiAI as White

main :: IO ()
main = GameEngine.play
  (Black.author, Black.nickname, Black.initial, Black.think)
  (White.author, White.nickname, White.initial, White.think)
