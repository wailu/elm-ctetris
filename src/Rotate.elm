module Rotate exposing (rotateI, rotateJ, rotateL, rotateS, rotateT, rotateZ)


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
