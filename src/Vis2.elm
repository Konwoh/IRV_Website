module Vis2 exposing (..)
import Data exposing (Attributes(..))
import Data exposing (Sale)
import Axis
import Color
import Html exposing (Html, text)
import Scale exposing (ContinuousScale)
import Statistics
import List.Extra
import TypedSvg exposing (circle, g, rect, style, svg, text_, ellipse, line)
import TypedSvg.Attributes exposing (class, color, fill, fontFamily, fontSize, stroke, textAnchor, transform, viewBox, fontWeight)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y, rx, ry, x1, x2, y1, y2, strokeWidth)
import TypedSvg.Core exposing (Attribute, Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..), px, percent)
import Tuple
import List
import Shape
import Path
import Vis1 exposing (wideExtent)
indexA: Int
indexA = 
    0

indexB: Int
indexB =
    0
padding : Float
padding =
    60

parallelCoordinates : Float -> Float -> MultiDimData -> List Sale-> Svg msg
parallelCoordinates w ar model saleList=
    let
        h : Float 
        h = w / ar 
        readModelList =
                model.data
                    |> List.concat
                    |> List.map .value
                    |> List.Extra.transpose
        

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
            List.map (Axis.left [ Axis.tickCount 8 ]) (swapListItem scaleList indexA indexB)


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
                    (swapListItem model.dimDescription indexA indexB)
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
                                    [scale1, scale2, scale3, scale4, scale5, scale6, scale7, scale8]
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
        
multiPoint : String -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> MultiDimPoint 
multiPoint pointName unit_price quantity tax total cogs gross_margin_percentage gross_income rating =
    MultiDimPoint pointName [unit_price, quantity, tax, total, cogs, gross_margin_percentage, gross_income, rating]

saleToMultiPoint: Data.Sale -> MultiDimPoint
saleToMultiPoint sale =
    multiPoint (sale.invoice_ID) (sale.unit_price) (sale.quantity) (sale.tax) (sale.total) (sale.cogs) (sale.gross_margin_percentage) (sale.gross_income) (sale.rating) 


type alias MultiDimPoint =
    { pointName : String, value : List Float }


type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }
