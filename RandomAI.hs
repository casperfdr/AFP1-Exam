-- DO NOT MODIFY THIS FILE
-- DO NOT SUBMIT THIS FILE

module RandomAI(State,author,nickname,initial,think) where

import Reversi
import ReversiImplementation

type State = ()

author :: String
author = "Tjark Weber"

nickname :: String
nickname = "random"

initial :: Reversi.Player -> State
initial = undefined -- The actual implementation is hidden. Do NOT modify this file!

think :: State -> Reversi.Move -> Double -> (Reversi.Move,State)
think = undefined -- The actual implementation is hidden. Do NOT modify this file!
