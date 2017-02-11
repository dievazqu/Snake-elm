module Subscription exposing (subscriptions)

import Model exposing (..)
import Msg exposing (..)
import Time exposing (..)
import Keyboard exposing (..)

timePerTick = 0.1

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
        [ Keyboard.downs KeyMsg ,
        Time.every (timePerTick*second) Tick]
