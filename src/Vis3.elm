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