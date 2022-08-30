module RecursivePattern.Helper exposing (drawTuplePosition, normalizeFloat, normalizeInt, scaleHeight, scaleWidth, getW, getH)


{-| This library provides support for creating pixel-oriented visualizations

# Definition
@docs RecursivePattern.Helper

-}

import Color
import List
import RecursivePattern exposing (Level(..), PixelPositon(..))
import Scale exposing (ContinuousScale)
import Scale.Color
import Statistics
import TypedSvg.Attributes
import TypedSvg.Core
import TypedSvg.Types

{-| get the width of a specific level
    
    getW (Level 1 0) == 1
-}
getW : Level -> Int
getW (Level w h) =
    w



{-| get the height of a specific level

    getH (Level 1 0) == 0
-}
getH : Level -> Int
getH (Level w h) =
    h

{-| helper function to scale the heigth of a pixel

    scaleWidth 10 (Level 1 1) == Scale.linear ( 0.0, 10.0 ) ( 0.0, 1.0 )
-}
scaleWidth : Int -> List Level -> ContinuousScale Float
scaleWidth w level =
    let
        maxWidth =
            level
                |> List.map getW
                |> List.product
    in
    Scale.linear ( 0.0, toFloat w ) ( 0.0, toFloat maxWidth )

{-| helper function to scale the heigth of a pixel

    scaleHeight 10 (Level 1 1) == Scale.linear ( 0.0, 10.0 ) ( 0.0, 1.0 )
-}
scaleHeight : Int -> List Level -> ContinuousScale Float
scaleHeight h level =
    let
        maxHeight =
            level
                |> List.map getH
                |> List.product
    in
    Scale.linear ( 0.0, toFloat h ) ( 0.0, toFloat maxHeight )


scaleIntoPixelForm : Int -> List Level -> ContinuousScale Float
scaleIntoPixelForm x level =
    let
        maxHeight =
            level
                |> List.map getH
                |> List.product

        maxWidth =
            level
                |> List.map getW
                |> List.product

        maxTest = max maxWidth maxHeight
    in
    Scale.linear ( 0.0, toFloat x ) ( 0.0, toFloat maxTest )
   


{-| helper function to normalize and scale data
    specific for integer values

    normalizeInt [10] == Scale.linear ( 0.0, 10.0 ) ( 0.0, 1.0 )
-}
normalizeInt : List Int -> ContinuousScale Float
normalizeInt data =
    data
        |> List.map toFloat
        |> Statistics.extent
        |> Maybe.withDefault ( 0, 1 )
        |> Scale.linear ( 0, 1 )

{-| helper function to normalize and scale data
    specific for integer values

    normalizeFloat [10.0] == Scale.linear ( 0.0, 10.0 ) ( 0.0, 1.0 )
-}
normalizeFloat : List Float -> ContinuousScale Float
normalizeFloat data =
    data
        |> Statistics.extent
        |> Maybe.withDefault ( 0, 1 )
        |> Scale.linear ( 0, 1 )

{-| helper function to define the position and size of a pixel

    drawTuplePosition (10, 10) [ Level 1 1 ] (PixelPositon 0 0) == [ TypedSvg.Attributes.x (px 0), TypedSvg.Attributes.y (px 0), TypedSvg.Attributes.width (px 10), TypedSvg.Attributes.height (px 10)]
-}
drawTuplePosition : ( Int, Int ) -> List Level -> PixelPositon -> List (TypedSvg.Core.Attribute msg)
drawTuplePosition ( width, height ) level (PixelPositon x y) =
    [ TypedSvg.Attributes.x <| TypedSvg.Types.Px <| Scale.convert (scaleWidth width level) <| toFloat x
    , TypedSvg.Attributes.y <| TypedSvg.Types.Px <| Scale.convert (scaleHeight height level) <| toFloat y
    , TypedSvg.Attributes.width <| TypedSvg.Types.Px <| Scale.convert (scaleWidth width level) 1
    , TypedSvg.Attributes.height <| TypedSvg.Types.Px <| Scale.convert (scaleHeight height level) 1
    ]
