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
import Data

tickCount: Int
tickCount = 
    5

padding: Float
padding =
    60
-- Breite --
w : Float
w =
    900

-- Höhe --
h : Float
h =
    450
-- Datenstruktur für Punkt im scatterplot --
type alias Point =
    { pointName : String, x : Float, y : Float }

--Datenstruktur für XYdatapoint --
type alias XYdatapoint =
    {xAxisName : String, yAxisName : String, data : List Point}

-- Dtaenstruktur Sale--

type Msg
    = SelectBranch Data.Branch
    | SelectCity Data.City
    | SelectCustomerType Data.Customer_type
    | SelectGender Data.Gender
    | SelectProductLine Data.Product_line
    | SelectPayment Data.Payment
    | SelectAttribute Data.AttributeSelector Data.Attributes


-- scatterplot Funktion --
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
        -- Festlegung für Aussehen der Kreise im scatterplot --
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
                , text_
                [ x 0
                , y -10
                , textAnchor AnchorMiddle
                ]
                [ text dataPoint.pointName ]
                ]
    in 
        svg [viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ] 
            [style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point text { display: none; }
            .point:hover text { display: inline; }""" ]
            -- x-Achse zeichnen --
            , g [transform [ Translate (padding - 1) (h - padding) ]]
                [ xAxis xValues
                , text_ [x 750
                        , y 30
                        , fontSize (px 15)]
                        [text model.xAxisName]
                ]
            -- y-Achse zeichnen --
            , g [ transform [ Translate (padding - 1) padding ] ]
                [yAxis yValues
                , text_ [ x -25
                        , y -10
                        , fontSize (px 15)]
                        [text model.yAxisName]
                ]
            -- anwenden der point Funktion und die Skalierung auf die Datenpunkte
            , g [ transform [ Translate padding padding ] ]
            (List.map (point xSkalierung ySkalierung) model.data)
            ]

-- größter und kleinster Wert einer Liste berechnen für den Datenbereich --            
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

-- abstands algorithmus berechnen --
abstand : (Float, Float) -> Float -> (Float, Float)
abstand (min, max) s =
    if min < 0 || min < s then 
        (0, max + s)
    else (min - s, max + s)

-- Skalierung der x-Werte --
xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )

-- Skalierung der y-Werte --
yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )

-- zeichnen der x - Achse --

xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)

-- zeichnen der y - Achse --

yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)

-- Point Datenstrukutr Grundgerüst --
toPoint : String -> Float -> Int -> Point
toPoint invoiceID unitPrice quantity=
    Point invoiceID unitPrice (toFloat quantity)

-- toPoint Funktion die wählbaren Parameter übergeben --
salesPoint: Data.Sale -> Point
salesPoint sale =
    toPoint sale.invoice_ID sale.unit_price sale.quantity 

-- XYdatapoint Datenstrukutr erstellt mit anwennden der salespointFunktion auf den sales Parameter --
filterSales: List Data.Sale -> XYdatapoint
filterSales sales =
    XYdatapoint "Unit price" "quantity" (List.map salesPoint sales)


--main: Html msg
--main=
    --Html.div[]
            --[scatterplot]


filterBranch: List Data.Sale -> Data.Branch -> List Data.Sale
filterBranch salesList branch=
    List.filter (\a -> a.branch == branch) salesList