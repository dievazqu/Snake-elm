import Model exposing (..)
import View exposing (view)
import Update exposing (update)
import Subscription exposing (subscriptions)
import Html exposing (program)

import Msg exposing (Msg)

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : (Model, Cmd Msg)
init = (Menu 0, Cmd.none)
