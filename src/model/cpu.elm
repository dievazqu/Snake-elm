module CPU exposing (cpuAction)

import Model exposing (..)
import Auxiliar exposing (..)
import Matrix exposing (..)

cpuAction : GameModel -> GameModel
cpuAction model =
  { model | players = mapFilter (newCpuDirection model) (\p -> p.id == 2) model.players}

clearMatrix : (Matrix Tile) -> Matrix Tile
clearMatrix matrix =
  Matrix.map (\x -> {x | visited = False}) matrix

newCpuDirection : GameModel -> Player -> Player
newCpuDirection model player =
  let
    cleanedBoard = { model | board = (clearMatrix model.board)}
    initNodes = startingNodes cleanedBoard player
    bfsAnswer = bfsTransversal cleanedBoard initNodes
  in
    case bfsAnswer of
      Nothing -> player
      Just dir -> changePlayerDir dir player

type alias BfsNode =
  { tile : Coordinate,
    lastDir : Coordinate,
    dir : Coordinate}



bfsTransversal : GameModel -> (List BfsNode) -> Maybe Coordinate
bfsTransversal model queue =
  case queue of
    [q] ->
      let
        nc = nextCoordinates q
        validnc = List.filter (validNode model) nc
      in
        if model.fruit == q.tile || List.length validnc == 0 then
          Just q.dir
        else
          bfsTransversal (visite model validnc)
           validnc
    (q::qs) ->
      let
        nc = nextCoordinates q
        validnc = List.filter (validNode model) nc
      in
        if model.fruit == q.tile then
          Just q.dir
        else
          bfsTransversal (visite model validnc)
           (qs++validnc)
    _ -> Nothing

visite : GameModel -> List BfsNode -> GameModel
visite model list =
  List.foldr (\n m ->
    { m |
      board = Matrix.update n.tile (\x -> { x | visited = True}) m.board})
       model list

validNode : GameModel -> BfsNode -> Bool
validNode model bn =
    let
      mtile = Matrix.get bn.tile model.board
    in
      case mtile of
        Just tile ->
          (tile.elem == Empty || tile.elem == Fruit) && not (tile.visited)
        Nothing -> False

nextCoordinates : BfsNode -> List BfsNode
nextCoordinates node =
  List.map (\x -> {
    tile = sum x node.tile,
    lastDir = x,
    dir = node.dir
  }) (validDirections node.lastDir)

startingNodes m p =
  let
    mh = List.head p.snake
  in
    case mh of
      Nothing -> []
      Just c ->
        List.filter (validNode m)
        (List.map (\x -> {
          tile = sum x c,
          lastDir = x,
          dir = x
        }) (validDirections p.dir))

validDirections : Coordinate -> List Coordinate
validDirections ld = (List.filter (\x -> not (op x == ld)) directions)

directions = [(0,1),(0,-1),(1,0),(-1,0)]
op (x, y) = (-x, -y)
diff (x, y) (w, z) = (x-w, y-z)
