recrusivePatternPlot model dateList totalList=
    let
        createDateDictNothing =
                    Dict.fromList createDateTuple
        createDateDictReal =
                    Dict.fromList (List.map2 Tuple.pair dateList totalList)
        dateData =
                    Dict.union createDateDictReal createDateDictNothing
        dateDataList =
                    Dict.toList dateData
        
        ( w, h ) =
                    ( 500, 500 )

        level =
            [ RecursivePattern.Level 5 1
            , RecursivePattern.Level 1 12
            , RecursivePattern.Level 4 1
            , RecursivePattern.Level 2 3
            , RecursivePattern.Level 3 3
            , RecursivePattern.Level 1 1
            ]

        pixelList : List RecursivePattern.PixelPositon
        pixelList =
            RecursivePattern.createPixelMap RecursivePattern.startPosition
                (RecursivePattern.augementLevel level)
                
        ourData : List (CurrentRecordedData msg)
        ourData =
            List.map2 (\a b -> RecordedData a b []) pixelList dateDataList
                
        drawPosition : CurrentRecordedData msg -> CurrentRecordedData msg
        drawPosition (RecursivePattern.RecordedData pixelPosition value _) =
            RecursivePattern.RecordedData
                pixelPosition
                value
                (RecursivePattern.Helper.drawTuplePosition ( w, h ) level pixelPosition)

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
                            (Scale.Color.tealBluesInterpolator
                                << Scale.convert
                                    (RecursivePattern.Helper.normalizeFloat currentData)
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
    

createDateList : List String
createDateList =
    let
        start =
            fromCalendarDate 2019 Jan 1

        end =
            fromCalendarDate 2019 Mar 30
    in
    List.map Date.toIsoString (range Day 1 start end)

createDateTuple =
    List.map (\x -> Tuple.pair x Nothing) createDateList