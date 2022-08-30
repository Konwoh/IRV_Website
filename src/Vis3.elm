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