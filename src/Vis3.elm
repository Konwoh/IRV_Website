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