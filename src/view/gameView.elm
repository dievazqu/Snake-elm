module GameView exposing (gameView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Msg exposing (..)
import Model exposing (..)
import Matrix exposing (..)
import Html.Events exposing (onClick)

gameView : GameModel -> Html Msg
gameView model =
  div [ wrapperStyle] [
    (scoreboards model),
    div [ centering ] (boardToDiv (Matrix.flatten model.board)),
    div [] [button [onClick GoToMenu,  style [("font-size", "16px"), ("margin-top", "440px")] ] [text "GO TO MENU"]]
    ]

wrapperStyle : Html.Attribute Msg
wrapperStyle =
    style [
    ("text-align", "center"),
    ("background-color", "gray"),
    ("height", "100%"),
    ("position", "fixed"),
    ("width", "100%")]

scoreboards : GameModel -> Html Msg
scoreboards model =
  case model.state of
    Playing ->
      div [ style [("text-align", "center"), ("font-size", "32px"), ("margin-top", "20px"), ("height", "80px")]] [
        div [ style [("color", "darkblue")]] [text ("Player 1: "++(toString (score 1 model)))],
        div [ style [("color",  "darkred")]] [text ("Player 2: "++(toString (score 2 model)))]
        ]
    Finished fs ->
      case fs of
        Tie -> div
          [style [("text-align", "center"), ("font-size", "48px"),
          ("margin-top", "20px"), ("color", "Black"), ("height", "80px")]]
          [text ("Game Finished: Tie")]
        PlayerWin p -> div
          [style [("text-align", "center"), ("font-size", "48px"),
           ("margin-top", "20px"), ("color", "Black"), ("height", "80px")]]
          [text ("Game Finished: Player "++(toString p)++" wins")]


score : Int -> GameModel -> Int
score num model =
  let
    p = List.filter (\p -> p.id == num) model.players
  in
    case p of
      [x] -> (List.length x.snake) - 4
      _ -> 0




boardToDiv : List Tile -> List (Html Msg)
boardToDiv list = List.map (\x -> div [styleTile x] []) list

(width, height) = (10, 10)
(deltaTop, deltaLeft) = (0, 0)

centering : Attribute Msg
centering = style
  [("position", "fixed"),
   ("left", "50%"),
   ("margin-top", "10px"),
   ("margin-left", "-200px")]

styleTile : Tile -> Attribute Msg
styleTile tile = style
  [("margin-top", toString (width*tile.row) ++ "px"),
   ("margin-left", toString (height*tile.col) ++ "px"),
   ("background-color", colorFromTile tile),
   ("position", "absolute"),
   ("width", "10px"),
   ("height", "10px")]



colorFromTile : Tile -> String
colorFromTile tile =
  case tile.elem of
    Empty -> "lightgray"
    Wall -> "black"
    Fruit -> "darkgreen"
    PlayerHead 1 -> "blue"
    PlayerTail 1 -> "darkblue"
    PlayerHead 2 -> "red"
    PlayerTail 2 -> "darkred"
    Collision -> "darkviolet"
    _ -> "white"
