module View exposing (view)

import Html exposing (..)
import Msg exposing (..)
import Model exposing (..)
import GameView exposing (gameView)
import MenuView exposing (menuView)

view : Model -> Html Msg
view model =
  case model of
    Menu _ -> menuView
    InGame g -> gameView g
