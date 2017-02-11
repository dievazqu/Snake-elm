module Update exposing (update)
import Model exposing (..)
import Msg exposing (..)
import CPU exposing (..)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case model of
    Menu time ->
      case msg of
       StartingGameVsHuman -> (InGame (initState time False), Cmd.none)
       StartingGameVsCPU -> (InGame (initState time True), Cmd.none)
       Tick newTime -> (Menu (round newTime), Cmd.none)
       _ -> (model, Cmd.none)
    InGame g ->
      let
        (ng, cmd) = inGameAction msg g
      in
        (InGame (vscpu ng (cpuAction ng)), cmd)

vshuman : GameModel -> GameModel -> GameModel
vshuman g m =
  if not g.cpu then
    m
  else
    g

inGameAction : Msg -> GameModel -> (GameModel, Cmd Msg)
inGameAction msg g =
  case msg of
    Tick newTime -> (maybeMakeMove g, Cmd.none)
    KeyMsg code ->
       case code of
       40 -> (maybeChangeDir g 1 (1, 0), Cmd.none)
       37 -> (maybeChangeDir g 1 (0, -1), Cmd.none)
       39 -> (maybeChangeDir g 1 (0, 1), Cmd.none)
       38 -> (maybeChangeDir g 1 (-1, 0), Cmd.none)
       65 -> (vshuman g (maybeChangeDir g 2 (0, -1)), Cmd.none)
       83 -> (vshuman g (maybeChangeDir g 2 (1, 0)), Cmd.none)
       68 -> (vshuman g (maybeChangeDir g 2 (0, 1)), Cmd.none)
       87 -> (vshuman g (maybeChangeDir g 2 (-1, 0)), Cmd.none)
       _ -> (g, Cmd.none )
    _ -> (g, Cmd.none)



vscpu : GameModel -> GameModel -> GameModel
vscpu g m =
  if g.cpu then
    m
  else
    g

maybeChangeDir : GameModel -> Int -> (Int, Int) -> GameModel
maybeChangeDir m n d =
  case m.state of
    Playing -> changeDir m n d
    _ -> m

maybeMakeMove : GameModel -> GameModel
maybeMakeMove m =
  case m.state of
    Playing -> makeMove m
    _ -> m
