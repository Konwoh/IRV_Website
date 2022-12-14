module Vis2 exposing (..)
import Data exposing (Attributes(..))
import Data exposing (Sale)
import Axis
import Color
import Scale
import List.Extra
import TypedSvg exposing ( g, style, svg, text_)
import TypedSvg.Attributes exposing (fill, fontFamily, fontSize, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing ( x, y, strokeWidth)
import TypedSvg.Core exposing ( Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..), px)
import List
import Shape
import Path
import Vis1 
indexA: Int
indexA = 
    0

indexB: Int
indexB =
    0
padding : Float
padding =
    60

parallelCoordinates : Float -> Float -> MultiDimData -> List Sale-> Int -> Int -> Svg msg
parallelCoordinates w ar model saleList index1 index2=
    let
        h : Float 
        h = w / ar 
    
        unit_priceFloats = List.map .unit_price saleList
        quantityFloats = List.map .quantity saleList
        taxFloats = List.map .tax saleList
        totalFloats = List.map .total saleList
        cogsFloats = List.map .cogs saleList
        gross_margin_percentageFloats = List.map .gross_margin_percentage saleList
        gross_incomeFloats = List.map .gross_income saleList
        ratingFloats = List.map .rating saleList

        scale1 =
            Scale.linear (h, 0) (Vis1.wideExtent unit_priceFloats)
        scale2 =
            Scale.linear (h, 0) (Vis1.wideExtent quantityFloats)
        scale3 =
            Scale.linear (h, 0) (Vis1.wideExtent taxFloats)
        scale4 =
            Scale.linear (h, 0) (Vis1.wideExtent totalFloats)
        scale5 =
            Scale.linear (h, 0) (Vis1.wideExtent cogsFloats)
        scale6 =
            Scale.linear (h, 0) (Vis1.wideExtent gross_margin_percentageFloats)
        scale7 =
            Scale.linear (h, 0) (Vis1.wideExtent gross_incomeFloats)
        scale8 =
            Scale.linear (h, 0) (Vis1.wideExtent ratingFloats)
        
        scaleX =
            Scale.linear (0, w) ( 1, 8)
        
        scaleList =
            [scale1, scale2, scale3, scale4, scale5, scale6, scale7, scale8]
        
        swapListItem l a b=
            List.Extra.swapAt a b l 


        listAxis =
            List.map (Axis.left [ Axis.tickCount 8 ]) (swapListItem scaleList index1 index2)


    in
    svg [ viewBox 0 0 (w + 2 * padding) (h + 2 * padding), TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 
        ]
    <|
        [ style [] 
            []
        , g [ transform [ Translate (padding - 1) padding ] ] <|
                List.indexedMap
                    (\i axis ->
                        g
                            [ transform
                                [ Translate (Scale.convert scaleX (toFloat i + 1)) 0
                                ]
                            ]
                            [ axis ]
                    )
                    listAxis
        , g [ transform [ Translate (padding - 1) 0 ] ] <|
                List.indexedMap
                    (\i desc ->
                        text_
                            [ fontFamily [ "sans-serif" ]
                            , fontSize (Px 10)
                            , x <| Scale.convert scaleX (toFloat i + 1)
                            , y <| padding * 7 / 8
                            , textAnchor AnchorMiddle
                            ]
                            [ TypedSvg.Core.text desc ]
                    )
                    (swapListItem model.dimDescription index1 index2)
        ] 
        ++ (let
                    drawPoint p =
                        let
                            linePath : Path.Path
                            linePath =
                                List.map3
                                    (\desc s px ->
                                        Just
                                            ( Scale.convert scaleX <| toFloat desc
                                            , Scale.convert s px
                                            )
                                    )
                                    (List.range 1 (List.length model.dimDescription))
                                    (swapListItem [scale1, scale2, scale3, scale4, scale5, scale6, scale7, scale8] index1 index2)
                                    p
                                    |> Shape.line Shape.linearCurve
                        in
                        Path.element linePath
                            [ stroke <| Paint <| Color.rgba 0 0 0 0.8
                            , strokeWidth <|  1
                            , fill PaintNone
                            ]
                in
                model.data
                    |> List.map
                        (\dataset ->
                            g [ transform [ Translate (padding - 1) padding ] ]
                                (List.map (.value >> drawPoint) dataset)
                        )
               )
        


type alias MultiDimPoint =
    { pointName : String, value : List Float }


type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }
