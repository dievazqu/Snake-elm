module CPU exposing (cpuAction)

import Model exposing (..)
import Auxiliar exposing (..)

cpuAction : GameModel -> GameModel
cpuAction m =
  { m | players = mapFilter (newCpuDirection m) (\p -> p.id == 2) m.players}

newCpuDirection : GameModel -> Player -> Player
newCpuDirection m p =
  let
    initNodes = startingNodes m p
    bfsAnswer = bfsTransversal m (List.map (\x->x.tile) initNodes) initNodes
  in
    case bfsAnswer of
      Nothing -> p
      Just dir -> changePlayerDir dir p

{-  let
    mh = List.head p.snake
  in
    case mh of
      Nothing -> p
      Just h ->
        let bfsAnswer = bfsTransversal m [h]
        in
          case bfsAnswer of
            Nothing -> p
            Just dir -> changePlayerDir dir p
            -}

type alias BfsNode =
  { tile : Coordinate,
    lastDir : Coordinate,
    dir : Coordinate}


-- Agrego a la queue solo los validos --
bfsTransversal : GameModel -> (List Coordinate) -> (List BfsNode) -> Maybe Coordinate
bfsTransversal model visited queue =
  case queue of
    [q] ->
      let
        nc = nextCoordinates q
        validnc = List.filter (validNode model visited) nc
      in
        if model.fruit == q.tile || List.length validnc == 0 then
          Just q.dir
        else
          bfsTransversal model
           (visited++(List.map (\x -> x.tile) validnc))
           validnc
    (q::qs) ->
      let
        nc = nextCoordinates q
        validnc = List.filter (validNode model visited) nc
      in
        if List.length queue >= 100 then
          Just q.dir
        else
        if model.fruit == q.tile then
          Just q.dir
        else
          bfsTransversal model
           (visited++(List.map (\x -> x.tile) validnc))
           (qs++validnc)
    _ -> Nothing

validNode : GameModel -> (List Coordinate) -> BfsNode -> Bool
validNode model visited bn =
    let
      mtile = List.filter (\a -> getCoordinate a == bn.tile) model.board
    in
      case mtile of
        [tile] ->
          (tile.elem == Empty || tile.elem == Fruit) && not (List.member bn.tile visited)
        _ -> False

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
        List.filter (validNode m [])
        (List.map (\x -> {
          tile = sum x c,
          lastDir = x,
          dir = x
        }) (validDirections p.dir))

validDirections : Coordinate -> List Coordinate
validDirections ld = (List.filter (\x -> not (op x == ld)) directions)

directions = [(0,1),(0,-1),(1,0),(-1,0)]
sum (x, y) (w, z) = (x+w, y+z)
op (x, y) = (-x, -y)
diff (x, y) (w, z) = (x-w, y-z)
