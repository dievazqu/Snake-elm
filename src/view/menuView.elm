module MenuView exposing (menuView)
import Html exposing (..)
import Msg exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Model exposing (..)

menuView : Html Msg
menuView =
  div [wrapperStyle] [
    div [ style [("margin-top", "24px"), ("font-size", "140px") ] ] [ text "Snake"],
    div [] [button [onClick StartingGameVsHuman, buttonsStyle ] [text "2 PLAYERS"]],
    div [] [button [onClick StartingGameVsCPU, buttonsStyle ] [text "VS CPU"]],
    div [ style [("margin-top", "24px"), ("font-size", "36px")]] [text "Controls:"],
    div [] [text "Player 1: Arrows"],
    div [] [text "Player 2: WASD"],
    div [ style [("margin-top", "24px"), ("font-size", "36px")]] [text "Objective:"],
    div [] [text ("Collect "++(toString winningPoints)++" fruits")]]


wrapperStyle : Html.Attribute Msg
wrapperStyle =
    style [
    ("text-align", "center"),
    ("background-color", "gray"),
    ("height", "100%"),
    ("position", "fixed"),
    ("width", "100%"),
    ("font-size", "24px")]


buttonsStyle : Html.Attribute Msg
buttonsStyle =
  style [
  ("margin-top", "24px"),
  ("width", "140px"),
  ("height", "60px"),
  ("font-size", "24px")]
