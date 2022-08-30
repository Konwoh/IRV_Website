module RecursivePattern exposing (..)module RecursivePattern exposing (Level(..), PixelPositon(..), RecordedData(..), augementLevel, createPixelMap, createPixelMapTopDown, createPixelMapZ, startPosition)

{-| This library provides the option of creating a list of pixel positions based on a specific order

# Definition
@docs RecursivePattern

-}


{-| type definition for the data and results used
    get data and result type to create useable type
    useable for chaning optional position functions
-}
type RecordedData value result
    = RecordedData PixelPositon value result

{-| helper type for the recursive loop call
    xOuterLoop -> x value of the outer loop
    yOuterLoop -> y value of the outer loop
    outerResult -> intermediate results
-}
type alias LoopRec =
    { xOuterLoop : Int, yOuterLoop : Int, outerResult : List PixelPositon }

{-| type definition for a pixel position
    first parameter -> defines the horizontal position of the pixel
    second parameter -> defines the vertical position of the pixel
-}
type PixelPositon
    = PixelPositon Int Int

{-| type definition for a recursive pattern level
    first parameter -> defines the number of pixels horizontally
    second parameter -> defines the number of pixels vertically
-}
type Level
    = Level Int Int

{-| constructor for the start value of the pixel positions
    affects the position within the representation 

    startPosition == (PixelPositon 0 0)
-}
startPosition : PixelPositon
startPosition =
    PixelPositon 0 0


{-| creates the augment level -> takes the last level and multiply them on the current level
    
    augmentLevel [(Level 2 2)] == [ ( Level 2 2, Level 1 1 ), ( Level 1 1, Level 0 0 ) ]
-}
augementLevel : List Level -> List ( Level, Level )
augementLevel level =
    let
        usableLevel =
            level ++ [ Level 1 1 ]

        next : List Level
        next =
            List.foldr
                -- change to foldl
                (\(Level currentLevelW currentLevelH) nextLevel ->
                    let
                        (Level one two) =
                            case nextLevel of
                                [] ->
                                    Level 1 1

                                (Level x y) :: _ ->
                                    Level x y
                    in
                    Level (currentLevelW * one) (currentLevelH * two) :: nextLevel
                )
                []
                usableLevel
    in
    List.map2 Tuple.pair usableLevel (List.append (List.drop 1 next) [ Level 0 0 ])


{-| creates a list of pixel positions that correspond to the left-right arrangement

    createPixelMap (PixelPosition 0 0) [ ( Level 2 2, Level 1 1 ), ( Level 1 1, Level 0 0 ) ] == [ PixelPositon 0 0, PixelPositon 1 0, PixelPositon 0 1, PixelPositon 1 1 ]
-}
createPixelMap : PixelPositon -> List ( Level, Level ) -> List PixelPositon
createPixelMap (PixelPositon x y) level =
    case level of
        [] ->
            [ PixelPositon x y ]

        ( Level w h, Level nextW nextH ) :: reducedLevel ->
            let
                temp =
                    List.range 1 h
                        |> List.foldl
                            (\_ { xOuterLoop, yOuterLoop, outerResult } ->
                                let
                                    innerResult =
                                        List.range 1 w
                                            |> List.foldl
                                                (\_ ( xLoop, result ) ->
                                                    ( xLoop + nextW
                                                    , List.append
                                                        result
                                                        (createPixelMap (PixelPositon xLoop yOuterLoop) reducedLevel)
                                                    )
                                                )
                                                ( xOuterLoop, [] )
                                in
                                LoopRec
                                    xOuterLoop
                                    (yOuterLoop + nextH)
                                    (List.append outerResult (Tuple.second innerResult))
                            )
                            (LoopRec x y [])
            in
            temp.outerResult

{-| creates a list of pixel positions that correspond to the top-down arrangement

    createPixelMapTopDown (PixelPosition 0 0) [ ( Level 2 2, Level 1 1 ), ( Level 1 1, Level 0 0 ) ] == [ PixelPositon 0 0, PixelPositon 0 1, PixelPositon 1 0, PixelPositon 1 1 ]
-}
createPixelMapTopDown : PixelPositon -> List ( Level, Level ) -> List PixelPositon
createPixelMapTopDown (PixelPositon x y) level =
    case level of
        [] ->
            [ PixelPositon x y ]

        ( Level w h, Level nextW nextH ) :: reducedLevel ->
            let
                temp =
                    List.range 1 w
                        |> List.foldl
                            (\_ { xOuterLoop, yOuterLoop, outerResult } ->
                                let
                                    innerResult =
                                        List.range 1 h
                                            |> List.foldl
                                                (\_ ( yLoop, result ) ->
                                                    ( yLoop + nextH
                                                    , List.append
                                                        result
                                                        (createPixelMapTopDown (PixelPositon xOuterLoop yLoop) reducedLevel)
                                                    )
                                                )
                                                ( yOuterLoop, [] )
                                in
                                LoopRec
                                    (xOuterLoop + nextW)
                                    yOuterLoop
                                    (List.append outerResult (Tuple.second innerResult))
                            )
                            (LoopRec x y [])
            in
            temp.outerResult

{-| creates a list of pixel positions that correspond to the back-and-forth arrangement
    
    createPixelMapZ (PixelPosition 0 0) [ ( Level 2 2, Level 1 1 ), ( Level 1 1, Level 0 0 ) ] == [ PixelPositon 0 0, PixelPositon 1 0, PixelPositon 1 1, PixelPositon 0 1 ]
-}
createPixelMapZ : PixelPositon -> List ( Level, Level ) -> List PixelPositon
createPixelMapZ (PixelPositon x y) level =
    case level of
        [] ->
            [ PixelPositon x y ]

        ( Level w h, Level nextW nextH ) :: reducedLevel ->
            let
                temp =
                    List.range 1 h
                        |> List.foldl
                            (\height { xOuterLoop, yOuterLoop, outerResult } ->
                                let
                                    isOdd =
                                        if h == 1 && nextH == 1 then
                                             modBy 2 yOuterLoop == 1
                                        
                                        else    
                                            modBy 2 height == 0

                                    innerResult =
                                        List.range 1 w
                                            |> List.foldl
                                                (\_ ( xLoop, result ) ->
                                                    let
                                                        xWert =
                                                            if isOdd then
                                                                xLoop - nextW

                                                            else
                                                                xLoop + nextW

                                                        nextX =
                                                            if isOdd then
                                                                xLoop + ((w - 1) * nextW)

                                                            else
                                                                xLoop
                                                    in
                                                    ( xWert, List.append result (createPixelMapZ (PixelPositon nextX yOuterLoop) reducedLevel) )
                                                )
                                                ( xOuterLoop, [] )
                                in
                                LoopRec
                                    xOuterLoop
                                    (yOuterLoop + nextH)
                                    (List.append outerResult (Tuple.second innerResult))
                            )
                            (LoopRec x y [])
            in
            temp.outerResult
