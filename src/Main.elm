module Main exposing (Model)

import Array exposing (..)
import Browser
import Html exposing (Html, div, h1, span, td)
import Html.Attributes exposing (style)
import Random
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (fill, height, width)
import Task
import Time



-- Main


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 200 Tick



-- Model


type alias Model =
    { board : Board
    }


emptyBoard : Board
emptyBoard =
    initialize 20 (\_ -> initialize 10 (\_ -> Nothing))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = emptyBoard
      }
    , Task.perform Tick Time.now
    )


type alias Board =
    Array Line


type alias Line =
    Array (Maybe Unit)


type alias Unit =
    { drawing : Svg Msg
    , still : Bool
    , xPosition : Int
    , yPosition : Int
    }


type Tetromino
    = I
    | O
    | T
    | S
    | Z
    | J
    | L



-- Update


type Msg
    = Tick Time.Posix
    | NewPoint ( Int, Int )


point : Random.Generator ( Int, Int )
point =
    Random.pair (Random.int 0 19) (Random.int 0 9)


newPoint : Cmd Msg
newPoint =
    Random.generate NewPoint point


setOccupied : Int -> Int -> Board -> Board
setOccupied x y board =
    let
        testUnit =
            { drawing =
                svg
                    [ width "20", height "20" ]
                    [ rect [ height "20", width "20", fill "white" ] [] ]
            , still = False
            , xPosition = 0
            , yPosition = 4
            }
    in
    board
        |> get x
        |> Maybe.map (\row -> set y (Just testUnit) row)
        |> Maybe.map (\col -> set x col board)
        |> Maybe.withDefault board


draw : List ( Int, Int ) -> Board -> Board
draw lst board =
    let
        newLst =
            List.drop 1 lst

        maybeHead =
            List.head lst
    in
    maybeHead |> Maybe.map (\( x, y ) -> setOccupied x y board) |> Maybe.map (\nb -> draw newLst nb) |> Maybe.withDefault board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPoint ( x, y ) ->
            ( { model
                | board = setOccupied x y model.board
              }
            , Cmd.none
            )

        Tick _ ->
            ( { model | board = model.board }, newPoint )



-- View


renderLine : Array (Html Msg) -> Html Msg
renderLine arr =
    div [ style "display" "flex" ] (toList arr)


renderUnit : Maybe Unit -> Html Msg
renderUnit piece =
    case piece of
        Nothing ->
            svg [ width "20", height "20" ] [ rect [ height "20", width "20", fill "black" ] [] ]

        Just x ->
            x.drawing


collateLines : Array (Html Msg) -> Html Msg
collateLines arr =
    div [] (toList arr)


renderBoard : Board -> Html Msg
renderBoard board =
    board
        |> Array.map (Array.map renderUnit)
        |> Array.map renderLine
        |> collateLines


view : Model -> Html Msg
view model =
    div [] [ renderBoard (Debug.log "here" model.board) ]
