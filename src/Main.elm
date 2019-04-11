module Main exposing (Model)

import Array exposing (..)
import Browser
import Browser.Events as Events
import Html exposing (Html, div, h1, span, td)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Platform.Sub as Sub
import Random
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (fill, height, width)
import Task
import Time



-- Issues
{- model.moving_piece looks redundant
   it hangs when "game over" cause of the inifinite call to update
   can "shift" into other blocks
   can go through walls
-}
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
    Sub.batch [ Time.every 100 Tick, Events.onKeyDown keyDecoder ]



-- Model


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toControl (Decode.field "key" Decode.string)


type Key
    = Left
    | Right
    | Other


toControl : String -> Msg
toControl string =
    case string of
        "ArrowLeft" ->
            Control Left

        "ArrowRight" ->
            Control Right

        _ ->
            Control Other


type alias Model =
    { board : Board
    , moving_piece : List ( Int, Int )
    }


emptyBoard : Board
emptyBoard =
    initialize 20 (\_ -> initialize 10 (\_ -> Nothing))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = emptyBoard, moving_piece = [] }, nextPiece )


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
    | Gravity (List ( Int, Int ))
    | NewTetrominoPiece Tetromino
    | Control Key


tetromino : Random.Generator Tetromino
tetromino =
    Random.uniform I [ O, T, S, Z, J, L ]


nextPiece : Cmd Msg
nextPiece =
    Random.generate NewTetrominoPiece tetromino


isPresent : Maybe a -> Bool
isPresent m =
    case m of
        Nothing ->
            False

        _ ->
            True


getUnit : Int -> Int -> Board -> Maybe Unit
getUnit x y board =
    board
        |> get y
        |> Maybe.map (\line -> Maybe.withDefault Nothing (get x line))
        |> Maybe.withDefault Nothing


setOccupied : Int -> Int -> Bool -> Board -> Board
setOccupied x y isStill board =
    let
        testUnit =
            { drawing =
                svg
                    [ width "20", height "20" ]
                    [ rect [ height "20", width "20", fill "white" ] [] ]
            , still = isStill

            -- these numbers mean nothing as of now, perhaps its time to remove them!
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
    maybeHead
        |> Maybe.map (\( x, y ) -> setOccupied x y isStill board)
        |> Maybe.map (\nb -> draw newLst isStill nb)
        |> Maybe.withDefault board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTetrominoPiece piece ->
            case piece of
                I ->
                    ( { model
                        | moving_piece =
                            [ ( -4, 5 ), ( -3, 5 ), ( -2, 5 ), ( -1, 5 ) ]
                      }
                    , Cmd.none
                    )

                O ->
                    ( { model
                        | moving_piece =
                            [ ( -4, 5 ), ( -3, 5 ), ( -4, 6 ), ( -3, 6 ) ]
                      }
                    , Cmd.none
                    )

                T ->
                    ( { model
                        | moving_piece =
                            [ ( -4, 4 ), ( -4, 5 ), ( -4, 6 ), ( -3, 5 ) ]
                      }
                    , Cmd.none
                    )

                S ->
                    ( { model
                        | moving_piece =
                            [ ( -4, 5 ), ( -4, 6 ), ( -3, 6 ), ( -3, 7 ) ]
                      }
                    , Cmd.none
                    )

                Z ->
                    ( { model
                        | moving_piece =
                            [ ( -3, 5 ), ( -3, 6 ), ( -4, 6 ), ( -4, 7 ) ]
                      }
                    , Cmd.none
                    )

                J ->
                    ( { model
                        | moving_piece =
                            [ ( -3, 5 ), ( -4, 5 ), ( -4, 6 ), ( -4, 7 ) ]
                      }
                    , Cmd.none
                    )

                L ->
                    ( { model
                        | moving_piece =
                            [ ( -4, 5 ), ( -4, 6 ), ( -4, 7 ), ( -3, 7 ) ]
                      }
                    , Cmd.none
                    )

        Tick _ ->
            update (Gravity model.moving_piece) model

        Control key ->
            case key of
                Right ->
                    ( { model | moving_piece = List.map (\( y, x ) -> ( y, x + 1 )) model.moving_piece }, Cmd.none )

                Left ->
                    ( { model | moving_piece = List.map (\( y, x ) -> ( y, x - 1 )) model.moving_piece }, Cmd.none )

                Other ->
                    ( model, Cmd.none )

        Gravity xs ->
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

                possibleNextPos : List ( Int, Int )
                possibleNextPos =
                    xs |> List.map (\( y, x ) -> ( y + 1, x ))

                reachBottom : Bool
                reachBottom =
                    (xs |> List.filter (\( y, x ) -> y < 19) |> List.length)
                        /= 4

                hitStuff : Bool
                hitStuff =
                    List.length (possibleNextPos |> List.filter (\( y, x ) -> not (isPresent (getUnit x y stillBoard)))) /= 4

                newBoard =
                    if not reachBottom && not hitStuff then
                        Debug.log "what" (xs |> List.map (\( y, x ) -> ( y + 1, x )) |> draw) False stillBoard

                    else
                        (xs |> draw) True stillBoard

                movedCoordinates =
                    if reachBottom || hitStuff then
                        []

                    else
                        xs |> List.map (\( y, x ) -> ( y + 1, x ))

                newModel : Model
                newModel =
                    { model | board = newBoard, moving_piece = movedCoordinates }

                test =
                    Debug.log "test"
                        (getUnit 16 5 newBoard)

                test2 =
                    Debug.log
                        "test2"
                        (getUnit 5 16 newBoard)
            in
            if List.isEmpty movedCoordinates then
                ( newModel, nextPiece )

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
