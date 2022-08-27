parallelCoordinates : FLoat -> Float -> MultiDimData -> Svg Msg
parallelCoordinates w ar model =
    readModelList =
            model.data
                |> List.concat
                |> List.map .value
                |> List.Extra.transpose