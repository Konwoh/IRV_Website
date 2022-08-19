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

scatterplot: List Point -> Svg msg
scatterplot model = 
    let
        xValues: List Float
        xValues =
            List.map .x model

        yValues: List Float
        yValues =
            List.map .y model
        
        xSkalierung : ContinuousScale Float
        xSkalierung =
            xScale xValues

        ySkalierung : ContinuousScale Float
        ySkalierung =
            yScale yValues


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