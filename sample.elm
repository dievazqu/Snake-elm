
import Html exposing (Html, div, text, program)
import Keyboard
import Char exposing (..)
import Time exposing (Time, second)


-- MODEL


type alias Model =
    { s:String, t: Time}


init : ( Model, Cmd Msg )
init =
    ( {s="nada", t=0}, Cmd.none )



-- MESSAGES


type Msg
    = KeyMsg Keyboard.KeyCode | Tick Time



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text (toString model.t),
        div [] [text (toString model.s)] ]





-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg code ->
           case code of
           40 -> ({model | s="abajo"}, Cmd.none)
           37 -> ({model | s="izquierda"}, Cmd.none)
           39 -> ({model | s="derecha"}, Cmd.none)
           38 -> ({model | s="arriba"}, Cmd.none)
           65 -> ({model | s="izq"}, Cmd.none)
           83 -> ({model | s="abaj"}, Cmd.none)
           68 -> ({model | s="der"}, Cmd.none)
           87 -> ({model | s="arr"}, Cmd.none)
           _ -> ( {model | s=(toString code)}, Cmd.none )
        Tick newTime ->
          ({ model | t = newTime}, Cmd.none)




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg ,
        Time.every (0.5*second) Tick]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
