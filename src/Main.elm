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
    Time.every 1000 Tick



-- Model


type alias Model =
    { board : Board
    }


initUnit : Int -> Int -> Maybe Unit
initUnit i j =
    Nothing


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = initialize 20 (\_ -> initialize 10 (\_ -> Nothing))
      }
    , Task.perform Tick Time.now
    )


type alias Board =
    Array Line


type alias Line =
    Array (Maybe Unit)


type alias Unit =
    Svg Msg


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick x ->
            let
                testUnit =
                    Just
                        (svg
                            [ width "20", height "20" ]
                            [ rect [ height "20", width "20", fill "white" ] [] ]
                        )
            in
            Debug.log "tick"
                ( { model
                    | board =
                        model.board
                            |> get 0
                            |> Maybe.map (\line -> set 2 testUnit line)
                            |> Maybe.map (\line -> set 2 line model.board)
                            |> Maybe.withDefault model.board
                  }
                , Cmd.none
                )



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
            x


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
