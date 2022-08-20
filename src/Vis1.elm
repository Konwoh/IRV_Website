module Vis1 exposing (..)

import Axis
import Html exposing (Html, text)
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..), px)

tickCount: Int
tickCount = 
    5

padding: Float
padding =
    60

w : Float
w =
    900


h : Float
h =
    450

type alias Point =
    { pointName : String, x : Float, y : Float }

type alias XYdatapoint =
    {xAxisName : String, yAxisName : String, data : List Point}

type alias Sale =
  { invoice_ID : String
  , branch : String
  , city : String
  , customer_type : String
  , gender : String
  , product_line : String
  , unit_price : Float
  , quantity : Int
  , tax : Float
  , total : Float
  , date : String
  , time : String
  , payment : String
  , cogs : Float
  , gross_margin_percentage : Float
  , gross_income : Float
  , rating : Float
  }


scatterplot: XYdatapoint -> Svg msg
scatterplot model = 
    let
        xValues: List Float
        xValues =
            List.map .x model.data

        yValues: List Float
        yValues =
            List.map .y model.data
        
        xSkalierung : ContinuousScale Float
        xSkalierung =
            xScale xValues

        ySkalierung : ContinuousScale Float
        ySkalierung =
            yScale yValues
        
        point : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
        point scaleX scaleY dataPoint =
            g [class ["point"]  
               , fontSize <| Px 10.0
               , fontFamily [ "sans-serif" ]
               , transform
                    [ Translate
                        (Scale.convert scaleX dataPoint.x)
                        (Scale.convert scaleY dataPoint.y)
                    ]
              ]
                [ circle
                    [ cx 0
                    , cy 0
                    , r 5
                    ] []
                ]
    
    in 
        svg [viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ] 
            [style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }""" ]
            , g [transform [ Translate (padding - 1) (h - padding) ]]
                [xAxis xValues]
            , g [ transform [ Translate (padding - 1) padding ] ]
                [yAxis yValues]
            , g [ transform [ Translate padding padding ] ]
            (List.map (point xSkalierung ySkalierung) model.data)
            ]
wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let res1 = Maybe.withDefault (0,0)
                    (Statistics.extent values)
        max2 = Maybe.withDefault 0
                    (List.maximum values)
        min2 = Maybe.withDefault 0
                    (List.minimum values)
        res2 = abstand res1 ((max2 - min2)/(2*(toFloat tickCount)))
    in res2 

abstand : (Float, Float) -> Float -> (Float, Float)
abstand (min, max) s =
    if min < 0 || min < s then 
        (0, max + s)
    else (min - s, max + s)


xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )

xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)

toPoint : String -> Float -> Int -> Point
toPoint invoiceID unitPrice quantity=
    Point invoiceID unitPrice (toFloat quantity)

salesPoint: Sale -> Point
salesPoint sale =
    toPoint sale.invoice_ID sale.unit_price sale.quantity 

filterSales: List Sale -> XYdatapoint
filterSales sales =
    XYdatapoint "Unit price" "quantity" (List.map salesPoint sales)


--main: Html msg
--main=
    --Html.div[]
            --[scatterplot]