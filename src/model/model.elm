module Model exposing (..)

import Auxiliar exposing (..)
import Random
import Time
import Matrix exposing (..)

(rows, cols) = (40, 40)
winningPoints = 10

startingPos : Int -> List Coordinate
startingPos n =
  case n of
    1 -> [(18, 10), (19, 10), (20, 10), (21, 10)]
    2 -> [(21, 30), (20, 30), (19, 30), (18, 30)]
    _ -> []

type Model = Menu Int | InGame GameModel

type alias GameModel =
  {
    board: Matrix Tile,
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
type alias Tile = {row: Int, col: Int, elem: TileElement, visited : Bool}

getCoordinate : Tile -> Coordinate
getCoordinate tile =
  (tile.row, tile.col)


initState : Int -> Bool -> GameModel
initState s b= updateBoard {board = initBoard,
  players = [Player 1 (startingPos 1) (-1, 0),
  Player 2 (startingPos 2) (1, 0)],
  fruit = (20, 20),
  seed = Random.initialSeed s,
  cpu = b,
  state = Playing}

updateBoard : GameModel -> GameModel
updateBoard model =
  { model | board = Matrix.map (\t -> { t | elem = elemInTile model t}) model.board}

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

initBoard : Matrix Tile
initBoard = Matrix.matrix rows cols (\(x,y) -> {row=x, col=y, elem=Empty, visited=False})

randomCoordinate : Random.Generator Coordinate
randomCoordinate =
    Random.pair (Random.int 1 (rows-2)) (Random.int 1 (cols-2))

makeMove : GameModel -> GameModel
makeMove model =
  let
    modelAfterMove = updateBoard (checkFruit (movePlayers model))
  in
    if List.any (\t -> t.elem == Collision) (Matrix.flatten modelAfterMove.board)
      || (List.any (\t -> List.length t.snake >= winningPoints+4) model.players) then
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
    mc1 = getFromList 0 l
    mc2 = getFromList 1 l
  in
    case mc1 of
      Nothing -> (0, 0)
      Just (x, y) ->
        case mc2 of
          Nothing -> (0, 0)
          Just (w, z) ->
            (x-w, y-z)
