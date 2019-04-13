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
   can go through walls
   at slow tick rate key presses lags
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
    , moving_piece : ( List ( Int, Int ), List ( Int, Int ) -> List ( Int, Int ) )
    }


emptyBoard : Board
emptyBoard =
    initialize 20 (\_ -> initialize 10 (\_ -> Nothing))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = emptyBoard, moving_piece = ( [], \x -> x ) }, nextPiece )


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
    | Gravity ( List ( Int, Int ), List ( Int, Int ) -> List ( Int, Int ) )
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


hitStuff : List ( Int, Int ) -> Board -> Bool
hitStuff possibleNextPos stillBoard =
    List.length (possibleNextPos |> List.filter (\( y, x ) -> not (isPresent (getUnit x y stillBoard)))) /= 4


reachBottom : List ( Int, Int ) -> Bool
reachBottom xs =
    (xs |> List.filter (\( y, x ) -> y < 19) |> List.length)
        /= 4


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


getStillBoard : Board -> Board
getStillBoard board =
    board |> map (\x -> map (\y -> eraseNonStill y) x)


clearLine : Board -> Board
clearLine board =
    let
        lineFull : Array (Maybe Unit) -> Bool
        lineFull arr =
            Debug.log "length" (filter (\sq -> isPresent sq) arr |> length) == 10

        clear =
            board |> filter (\line -> not (lineFull line))

        clearedLines : Int
        clearedLines =
            Debug.log "clearedlines" (20 - length clear)
    in
    append (initialize clearedLines (\_ -> initialize 10 (\_ -> Nothing))) clear


rotateI : List ( Int, Int ) -> List ( Int, Int )
rotateI xs =
    let
        isVertical : Bool
        isVertical =
            (List.head xs
                |> Maybe.map (\( b, a ) -> List.filter (\( y, x ) -> x == a) xs)
                |> Maybe.map List.length
            )
                |> Maybe.map (\x -> x /= 4)
                |> Maybe.withDefault False
    in
    if isVertical then
        (\list -> List.indexedMap (\n -> \( y, x ) -> ( y - n + 1, x + n - 1 )) list) xs

    else
        (\list -> List.indexedMap (\n -> \( y, x ) -> ( y + n - 1, x - n + 1 )) list) xs


rotateT : List ( Int, Int ) -> List ( Int, Int )
rotateT xs =
    let
        maybeHead =
            Debug.log "originalllllllllhead" (List.head xs)

        maybeTail =
            Debug.log "originalllllllllltail" (List.tail xs)
    in
    Maybe.map2
        (\( hy, hx ) ->
            \list ->
                List.map
                    (\( ty, tx ) ->
                        if ty > hy && tx == hx then
                            ( ty - 1, tx - 1 )

                        else if ty == hy && tx < hx then
                            ( ty - 1, tx + 1 )

                        else if ty < hy && tx == hx then
                            ( ty + 1, tx + 1 )

                        else
                            ( ty + 1, tx - 1 )
                    )
                    (Maybe.withDefault [] maybeTail)
        )
        maybeHead
        maybeTail
        |> Maybe.map2 (\head -> \tail -> head :: tail) maybeHead
        |> Maybe.withDefault []


rotateZ : List ( Int, Int ) -> List ( Int, Int )
rotateZ xs =
    let
        maybeHead =
            List.head xs

        maybeTail =
            Maybe.Just (List.drop 2 xs)

        list1 =
            List.take 2 xs
    in
    Maybe.map2
        (\( hy, hx ) ->
            \list ->
                List.map
                    (\( ty, tx ) ->
                        if ty > hy && tx > hx then
                            ( ty - 2, tx )

                        else if ty == hy && tx < hx then
                            ( ty, tx + 2 )

                        else if ty == hy && tx > hx then
                            ( ty, tx - 2 )

                        else
                            ( ty + 2, tx )
                    )
                    (Maybe.withDefault [] maybeTail)
        )
        maybeHead
        maybeTail
        |> Maybe.map (\list2 -> List.append list1 list2)
        |> Maybe.withDefault []


rotateS : List ( Int, Int ) -> List ( Int, Int )
rotateS xs =
    let
        maybeHead =
            List.head xs

        maybeTail =
            Maybe.Just (List.drop 2 xs)

        list1 =
            List.take 2 xs
    in
    Maybe.map2
        (\( hy, hx ) ->
            \list ->
                List.map
                    (\( ty, tx ) ->
                        if ty > hy && tx == hx then
                            ( ty - 2, tx )

                        else if ty > hy && tx < hx then
                            ( ty, tx + 2 )

                        else if ty < hy && tx == hx then
                            ( ty + 2, tx )

                        else
                            ( ty, tx - 2 )
                    )
                    (Maybe.withDefault [] maybeTail)
        )
        maybeHead
        maybeTail
        |> Maybe.map (\list2 -> List.append list1 list2)
        |> Maybe.withDefault []


rotateJ : List ( Int, Int ) -> List ( Int, Int )
rotateJ xs =
    let
        maybeHead =
            List.head xs

        maybeTail =
            List.tail xs

        isTopHalf : Bool
        isTopHalf =
            Maybe.map2 (\( y, x ) -> \list -> List.filter (\( b, a ) -> b < y) list) maybeHead maybeTail
                |> Maybe.map (\list -> List.isEmpty list)
                |> Maybe.withDefault False

        isVertical : Bool
        isVertical =
            maybeTail
                |> Maybe.map (\list -> List.head list)
                |> Maybe.withDefault Nothing
                |> Maybe.map (\( b, a ) -> List.filter (\( y, x ) -> x == a) xs)
                |> Maybe.map List.length
                |> Maybe.map (\x -> x == 3)
                |> Maybe.withDefault False

        madeHorizontal : Maybe (List ( Int, Int ))
        madeHorizontal =
            maybeTail |> Maybe.map (\list -> List.head list) |> Maybe.withDefault Nothing |> Maybe.map (\( y, x ) -> ( y, x ) :: ( y, x - 1 ) :: ( y, x + 1 ) :: [])

        madeVertical : Maybe (List ( Int, Int ))
        madeVertical =
            maybeTail |> Maybe.map (\list -> List.head list) |> Maybe.withDefault Nothing |> Maybe.map (\( y, x ) -> ( y, x ) :: ( y - 1, x ) :: ( y + 1, x ) :: [])
    in
    if Debug.log "isVertical" isVertical && isTopHalf then
        Maybe.map2 (\( y, x ) -> \list -> ( y + 2, x ) :: list) maybeHead madeHorizontal
            |> Maybe.withDefault []

    else if isVertical then
        Maybe.map2 (\( y, x ) -> \list -> ( y - 2, x ) :: list) maybeHead madeHorizontal
            |> Maybe.withDefault []

    else if isTopHalf then
        Maybe.map2 (\( y, x ) -> \list -> ( y, x + 2 ) :: list) maybeHead madeVertical
            |> Maybe.withDefault []

    else
        Maybe.map2 (\( y, x ) -> \list -> ( y, x - 2 ) :: list) maybeHead madeVertical
            |> Maybe.withDefault []


rotateL : List ( Int, Int ) -> List ( Int, Int )
rotateL xs =
    let
        maybeHead =
            List.head xs

        maybeTail =
            List.tail xs

        isTopHalf : Bool
        isTopHalf =
            Maybe.map2 (\( y, x ) -> \list -> List.filter (\( b, a ) -> b < y) list) maybeHead maybeTail
                |> Maybe.map (\list -> List.isEmpty list)
                |> Maybe.withDefault False

        isVertical : Bool
        isVertical =
            maybeTail
                |> Maybe.map (\list -> List.head list)
                |> Maybe.withDefault Nothing
                |> Maybe.map (\( b, a ) -> List.filter (\( y, x ) -> x == a) xs)
                |> Maybe.map List.length
                |> Maybe.map (\x -> x == 3)
                |> Maybe.withDefault False

        madeHorizontal : Maybe (List ( Int, Int ))
        madeHorizontal =
            maybeTail |> Maybe.map (\list -> List.head list) |> Maybe.withDefault Nothing |> Maybe.map (\( y, x ) -> ( y, x ) :: ( y, x - 1 ) :: ( y, x + 1 ) :: [])

        madeVertical : Maybe (List ( Int, Int ))
        madeVertical =
            maybeTail |> Maybe.map (\list -> List.head list) |> Maybe.withDefault Nothing |> Maybe.map (\( y, x ) -> ( y, x ) :: ( y - 1, x ) :: ( y + 1, x ) :: [])
    in
    if Debug.log "isVertical" isVertical && isTopHalf then
        Maybe.map2 (\( y, x ) -> \list -> ( y, x + 2 ) :: list) maybeHead madeHorizontal
            |> Maybe.withDefault []

    else if isVertical then
        Maybe.map2 (\( y, x ) -> \list -> ( y, x - 2 ) :: list) maybeHead madeHorizontal
            |> Maybe.withDefault []

    else if isTopHalf then
        Maybe.map2 (\( y, x ) -> \list -> ( y + 2, x ) :: list) maybeHead madeVertical
            |> Maybe.withDefault []

    else
        Maybe.map2 (\( y, x ) -> \list -> ( y - 2, x ) :: list) maybeHead madeVertical
            |> Maybe.withDefault []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTetrominoPiece piece ->
            case piece of
                I ->
                    ( { model
                        | moving_piece =
                            ( [ ( -1, 5 ), ( -2, 5 ), ( -3, 5 ), ( -4, 5 ) ]
                            , rotateI
                            )
                      }
                    , Cmd.none
                    )

                O ->
                    ( { model
                        | moving_piece =
                            ( [ ( -4, 5 ), ( -3, 5 ), ( -4, 6 ), ( -3, 6 ) ], \list -> list )
                      }
                    , Cmd.none
                    )

                T ->
                    ( { model
                        | moving_piece =
                            ( [ ( -4, 5 ), ( -4, 6 ), ( -4, 4 ), ( -3, 5 ) ], rotateT )
                      }
                    , Cmd.none
                    )

                Z ->
                    ( { model
                        | moving_piece =
                            ( [ ( -4, 6 ), ( -3, 6 ), ( -3, 7 ), ( -4, 5 ) ], rotateZ )
                      }
                    , Cmd.none
                    )

                S ->
                    ( { model
                        | moving_piece =
                            ( [ ( -4, 6 ), ( -4, 7 ), ( -3, 6 ), ( -3, 5 ) ], rotateS )
                      }
                    , Cmd.none
                    )

                L ->
                    ( { model
                        | moving_piece =
                            ( [ ( -3, 5 ), ( -4, 6 ), ( -4, 5 ), ( -4, 7 ) ], rotateL )
                      }
                    , Cmd.none
                    )

                J ->
                    ( { model
                        | moving_piece =
                            ( [ ( -3, 7 ), ( -4, 6 ), ( -4, 5 ), ( -4, 7 ) ], rotateJ )
                      }
                    , Cmd.none
                    )

        Tick _ ->
            update (Gravity model.moving_piece) model

        Control key ->
            case key of
                Right ->
                    let
                        ( coordinates, f ) =
                            model.moving_piece

                        possibleNextPos =
                            List.map (\( y, x ) -> ( y, x + 1 )) coordinates
                    in
                    if hitStuff possibleNextPos (getStillBoard model.board) then
                        ( model, Cmd.none )

                    else
                        ( { model | moving_piece = ( possibleNextPos, f ) }, Cmd.none )

                Left ->
                    let
                        ( coordinates, f ) =
                            model.moving_piece

                        possibleNextPos =
                            List.map (\( y, x ) -> ( y, x - 1 )) coordinates
                    in
                    if hitStuff possibleNextPos (getStillBoard model.board) then
                        ( model, Cmd.none )

                    else
                        ( { model | moving_piece = ( possibleNextPos, f ) }, Cmd.none )

                Other ->
                    let
                        ( coordinates, f ) =
                            model.moving_piece

                        original =
                            Debug.log "original" coordinates

                        after =
                            Debug.log "after" (f coordinates)

                        possibleNextPos =
                            f coordinates
                    in
                    if hitStuff possibleNextPos (getStillBoard model.board) || reachBottom possibleNextPos then
                        ( model, Cmd.none )

                    else
                        ( { model | moving_piece = ( f coordinates, f ) }, Cmd.none )

        Gravity ( xs, f ) ->
            let
                possibleNextPos : List ( Int, Int )
                possibleNextPos =
                    xs |> List.map (\( y, x ) -> ( y + 1, x ))

                stillBoard =
                    getStillBoard model.board

                newBoard =
                    if not (reachBottom xs) && not (hitStuff possibleNextPos stillBoard) then
                        Debug.log "what" (xs |> List.map (\( y, x ) -> ( y + 1, x )) |> draw) False stillBoard

                    else
                        (xs |> draw) True stillBoard

                movedCoordinates =
                    if reachBottom xs || hitStuff possibleNextPos stillBoard then
                        []

                    else
                        xs |> List.map (\( y, x ) -> ( y + 1, x ))

                newModel : Model
                newModel =
                    { model | board = newBoard, moving_piece = ( movedCoordinates, f ) }
            in
            if List.isEmpty movedCoordinates then
                ( { newModel | board = clearLine newBoard }, nextPiece )

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
