module View exposing (view)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Msg exposing (..)
import Model exposing (..)
import GameView exposing (gameView)

view : Model -> Html Msg
view model =
  case model of
    Menu _ -> menuView
    InGame g -> gameView g


menuView : Html Msg
menuView =
  div [wrapperStyle] [
    div [ style [("margin-top", "24px"), ("font-size", "140px") ] ] [ text "Snake"],
    div [] [button [onClick StartingGameVsHuman, buttonsStyle ] [text "2 PLAYERS"]],
    div [] [button [onClick StartingGameVsCPU, buttonsStyle ] [text "VS CPU"]],
    div [] [button [onClick Rules, buttonsStyle ] [text "RULES"]]
  ]

wrapperStyle : Html.Attribute Msg
wrapperStyle =
    style [
    ("text-align", "center"),
    ("background-color", "gray"),
    ("height", "100%"),
    ("position", "fixed"),
    ("width", "100%")]


buttonsStyle : Html.Attribute Msg
buttonsStyle =
  style [
  ("margin-top", "24px"),
  ("width", "140px"),
  ("height", "60px"),
  ("font-size", "24px")]
