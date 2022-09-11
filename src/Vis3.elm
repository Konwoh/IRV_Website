module Vis3 exposing(..)

import Html
import Data exposing (CurrentRecordedData)
import RecursivePattern exposing (..)
import Helper
import Scale
import Scale.Color
import Time exposing (Month(..))
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (height, viewBox, width, x, strokeWidth, stroke)
import TypedSvg.Core exposing (..)
import Date exposing (..)
import TypedSvg.Types exposing (Length(..), Paint(..), Length(..))
import Scale
import Scale.Color
import Color
import Statistics
recrusivePatternPlot tuple=
    let
        dateDataList =
                    tuple
        
        ( w, h ) =
                    ( 700, 500 )

        level =
            [ RecursivePattern.Level 89 1

            
            ]

        pixelList : List RecursivePattern.PixelPositon
        pixelList =
            RecursivePattern.createPixelMap RecursivePattern.startPosition
                (RecursivePattern.augementLevel level)
                
        ourData : List (CurrentRecordedData msg)
        ourData =
            List.map2 (\a b -> RecursivePattern.RecordedData a b []) pixelList dateDataList
                
        drawPosition : CurrentRecordedData msg -> CurrentRecordedData msg
        drawPosition (RecursivePattern.RecordedData pixelPosition value _) =
            RecursivePattern.RecordedData
                pixelPosition
                value
                (Helper.drawTuplePosition ( w, h ) level pixelPosition)

        currentData : List Float
        currentData =
            let
                mfList : List (Maybe Float)
                mfList =
                    ourData
                        |> List.map (\(RecursivePattern.RecordedData _ ( _, mf ) _) -> mf)

                result =
                    mfList |> List.filterMap identity
            in
            result

        createStyle : Maybe Float -> List (TypedSvg.Core.Attribute msg)
        createStyle value =
            [ TypedSvg.Attributes.title
                (Maybe.withDefault "N.A." (Maybe.map String.fromFloat value))
            , TypedSvg.Attributes.fill <|
                TypedSvg.Types.Paint <|
                    Maybe.withDefault Color.darkGray <|
                        Maybe.map
                            (Scale.Color.lightMultiInterpolator 
                                << Scale.convert
                                    (Helper.normalizeFloat currentData)
                            )
                            value
            ]

        drawStyle : CurrentRecordedData msg -> CurrentRecordedData msg
        drawStyle (RecursivePattern.RecordedData pixelPosition ( dateString, value ) attributeList) =
            RecursivePattern.RecordedData
                pixelPosition
                ( dateString, value )
                (List.append attributeList (createStyle value))

        draw_neu : CurrentRecordedData msg -> TypedSvg.Core.Svg msg
        draw_neu (RecursivePattern.RecordedData _ _ attributeList) =
            TypedSvg.rect
                attributeList
                []
    in
    Html.div[][svg
                [ viewBox -100 0 900 350, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
                [ g [] <| List.map (drawPosition >> drawStyle >> draw_neu) ourData ]]

createDateList : List String
createDateList =
    let
        start =
            Date.fromCalendarDate 2019 Jan 1

        end =
            Date.fromCalendarDate 2019 Mar 30
    in
    List.map Date.toIsoString (range Day 1 start end)

createDateTuple =
    List.map (\x -> Tuple.pair x Nothing) createDateList

colorContext value data =
    let
        floatList = List.map (\(a,b)-> Maybe.withDefault 0.0 b) data
        colorList = floatList
                        |> Statistics.extent
                        |> Maybe.withDefault ( 0, 1 )
                        |> Scale.linear ( 0, 1 )

    in
    

    svg
        [ width (Px 11), height (Px 11)
        ]
        [
          rect
              [ TypedSvg.Attributes.x <| TypedSvg.Types.Px <| 0
              , TypedSvg.Attributes.y <| TypedSvg.Types.Px <| 0
              , TypedSvg.Attributes.width <| TypedSvg.Types.Px <| 50
              , TypedSvg.Attributes.height <| TypedSvg.Types.Px <| 50
              , TypedSvg.Attributes.fill <|
                  TypedSvg.Types.Paint <|
                      (Scale.Color.lightMultiInterpolator << Scale.convert colorList) value
              , strokeWidth (Px 1)
              , stroke <| Paint <| Color.rgba 0 0 0 0.4
              ]
              []
        ]
