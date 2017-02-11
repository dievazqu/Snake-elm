module Model exposing (..)

import Auxiliar exposing (..)
import Random
import Time

(rows, cols) = (20, 20)

startingPos : Int -> List Coordinate
startingPos n =
  case n of
    1 -> [(13, 10), (14, 10), (15, 10), (16, 10)]
    2 -> [(16, 15), (15, 15), (14, 15), (13, 15)]
    _ -> []

type Model = Menu Int | InGame GameModel

type alias GameModel =
  {
    board: List Tile,
    players: List(Player),
    fruit: Coordinate,
    seed : Random.Seed,
    state : State,
    cpu: Bool
  }

type State = Playing | Finished

type alias Coordinate = (Int, Int)

type alias Player = {
  id : Int,
  snake : List Coordinate,
  dir : Coordinate
}

type TileElement = Empty | PlayerHead Int | PlayerTail Int |
   Fruit | Wall | Collision
type alias Tile = {row: Int, col: Int, elem: TileElement}

getCoordinate : Tile -> Coordinate
getCoordinate tile =
  (tile.row, tile.col)


initState : Int -> Bool -> GameModel
initState s b= updateBoard {board = initBoard,
  players = [Player 1 (startingPos 1) (-1, 0),
  Player 2 (startingPos 2) (1, 0)],
  fruit = (13, 13),
  seed = Random.initialSeed s,
  cpu = b,
  state = Playing}

updateBoard : GameModel -> GameModel
updateBoard model =
  { model | board = List.map (\t -> { t | elem = elemInTile model t}) model.board}

elemInTile : GameModel -> Tile -> TileElement
elemInTile model tile =
  let
      coor = (getCoordinate tile)
      p = List.filter (\p -> List.member coor p.snake) model.players
  in
  case p of
    [x] ->
      let
        mTail = List.tail x.snake
      in
        if Just coor == List.head x.snake then
          if borderTile tile then
            Collision
          else
            case mTail of
              Nothing -> Collision
              Just t ->
                if List.member coor t then
                  Collision
                else
                  PlayerHead x.id
        else
          PlayerTail x.id
    (x::xs) -> Collision
    _ ->
      if borderTile tile then
        Wall
      else if fruitTile model tile then
        Fruit
      else
        Empty

borderTile : Tile -> Bool
borderTile t =
  (t.col == 0) || (t.col == cols-1) ||
    (t.row == 0) || (t.row == rows-1)

fruitTile : GameModel -> Tile -> Bool
fruitTile model tile =
  getCoordinate tile == model.fruit

initBoard : List Tile
initBoard = List.map mapNumToTile (List.range 0 (rows*cols - 1))

mapNumToTile : Int -> Tile
mapNumToTile n =
  Tile (n//cols) (n%cols) Empty



randomCoordinate : Random.Generator Coordinate
randomCoordinate =
    Random.pair (Random.int 1 (rows-2)) (Random.int 1 (cols-2))

makeMove : GameModel -> GameModel
makeMove model =
  let
    modelAfterMove = updateBoard (checkFruit (movePlayers model))
  in
    if List.any (\t -> t.elem == Collision) modelAfterMove.board then
      { modelAfterMove | state = Finished}
    else
      modelAfterMove

checkFruit : GameModel -> GameModel
checkFruit model =
  let
    b = List.any (\p -> List.member model.fruit p.snake) model.players
  in
    if b then
      newFruitPosition model
    else
      model

newFruitPosition : GameModel -> GameModel
newFruitPosition model =
  let
    (rc, newSeed) = Random.step randomCoordinate model.seed
    b = List.any (\p -> List.member rc p.snake) model.players
  in
    if b then
      newFruitPosition { model | seed = newSeed }
    else
      { model | seed = newSeed , fruit = rc }

movePlayers : GameModel -> GameModel
movePlayers model =
    { model | players = (List.map (movePlayer model) model.players) }

movePlayer : GameModel -> Player -> Player
movePlayer model p =
  let
    auxList = (addFirst p.dir p.snake)
    mc = List.head auxList
  in
    case mc of
      Nothing -> p
      Just c ->
            if c == model.fruit then
              { p | snake = auxList}
            else
              { p | snake = removeLast auxList}

addFirst : Coordinate -> (List Coordinate) -> List Coordinate
addFirst dir l =
  case List.head l of
    Nothing -> l
    Just c -> (addC c dir)::l

changeDir : GameModel -> Int -> Coordinate -> GameModel
changeDir model n d =
  { model | players = mapFilter (changePlayerDir d) (\p -> p.id == n) model.players}

changePlayerDir : Coordinate -> Player -> Player
changePlayerDir d p =
  if addC d (getLastDir p.snake) == (0, 0) then
    p
  else
    { p | dir = d}


getLastDir : List Coordinate -> Coordinate
getLastDir l =
  let
    mc1 = get 0 l
    mc2 = get 1 l
  in
    case mc1 of
      Nothing -> (0, 0)
      Just (x, y) ->
        case mc2 of
          Nothing -> (0, 0)
          Just (w, z) ->
            (x-w, y-z)