module Msg exposing (..)
import Keyboard exposing (..)
import Time exposing (..)

type Msg = Tick Time | KeyMsg Keyboard.KeyCode |
  StartingGameVsHuman | StartingGameVsCPU | Rules
