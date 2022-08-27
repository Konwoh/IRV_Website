parallelCoordinates : FLoat -> Float -> MultiDimData -> Svg Msg
import Data exposing (Sale)
parallelCoordinates w ar model =
    readModelList =
            model.data
                |> List.concat
                |> List.map .value
                |> List.Extra.transpose

multiPoint : String -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> MultiDimPoint 
multiPoint pointName unit_price quantity tax total cogs gross_margin_percentage gross_income rating =
    MultiDimPoint pointName [unit_price, quantity, tax, total, cogs, gross_margin_percentage, gross_income, rating]

saleToMultiPoint: Sale -> MultiDimPoint
saleToMultiPoint sale =
    Maybe.map5 multiPoint (sale.unit_price) (sale.quantity) (sale.tax) (sale.total) (sale.cogs) (sale. gross_margin_percentage) (sale.gross_income) (sale.rating) 