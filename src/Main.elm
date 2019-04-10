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
    , moving_piece : List ( Int, Int )
    }


emptyBoard : Board
emptyBoard =
    initialize 20 (\_ -> initialize 10 (\_ -> Nothing))


init : () -> ( Model, Cmd Msg )
init _ =
    update (NewPoint ( 1, 2 )) { board = emptyBoard, moving_piece = [] }


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
-- Add a Shift Msg


type Msg
    = Tick Time.Posix
    | NewPoint ( Int, Int )
    | Gravity (List ( Int, Int ))


point : Random.Generator ( Int, Int )
point =
    Random.pair (Random.int 0 19) (Random.int 0 9)


newPoint : Cmd Msg
newPoint =
    Random.generate NewPoint point


setOccupied : Int -> Int -> Bool -> Board -> Board
setOccupied x y isStill board =
    let
        testUnit =
            { drawing =
                svg
                    [ width "20", height "20" ]
                    [ rect [ height "20", width "20", fill "white" ] [] ]
            , still = isStill
            , xPosition = 0
            , yPosition = 4
            }
    in
    board
        |> get x
        |> Maybe.map (\row -> set y (Just testUnit) row)
        |> Maybe.map (\col -> set x col board)
        |> Maybe.withDefault board


draw : List ( Int, Int ) -> Bool -> Board -> Board
draw lst isStill board =
    let
        newLst =
            List.drop 1 lst

        maybeHead =
            List.head lst
    in
    maybeHead |> Maybe.map (\( x, y ) -> setOccupied x y isStill board) |> Maybe.map (\nb -> draw newLst isStill nb) |> Maybe.withDefault board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPoint ( x, y ) ->
            let
                coordinates =
                    [ ( -4, 5 ), ( -3, 5 ), ( -2, 5 ), ( -1, 5 ) ]
            in
            update
                (Gravity coordinates)
                { model | moving_piece = coordinates }

        Tick _ ->
            let
                eraseNonStill : Maybe Unit -> Maybe Unit
                eraseNonStill smth =
                    case smth of
                        Just a ->
                            if a.still then
                                Just a

                            else
                                Nothing

                        Nothing ->
                            Nothing

                stillBoard : Board
                stillBoard =
                    model.board |> map (\x -> map (\y -> eraseNonStill y) x)
            in
            update (Gravity model.moving_piece) { model | board = stillBoard }

        Gravity xs ->
            let
                isStuck : Bool
                isStuck =
                    (model.moving_piece |> List.filter (\( y, x ) -> y < 19) |> List.length) /= 4

                newBoard =
                    if not isStuck then
                        Debug.log "what" (xs |> List.map (\( y, x ) -> ( y + 1, x )) |> draw) False model.board

                    else
                        (xs |> draw) True model.board

                movedCoordinates =
                    if isStuck then
                        []

                    else
                        xs |> List.map (\( y, x ) -> ( y + 1, x ))

                newModel : Model
                newModel =
                    { model | board = newBoard, moving_piece = movedCoordinates }
            in
            if List.isEmpty movedCoordinates then
                update (NewPoint ( 1, 2 )) newModel

            else
                ( newModel, Cmd.none )



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
