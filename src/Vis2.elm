module Vis2 exposing (..)
import Data exposing (Attributes(..))
import Data exposing (Sale)

indexA: Int
indexA = 
    0

indexB: Int
indexB =
    0

parallelCoordinates : Float -> Float -> MultiDimData -> Svg Msg
parallelCoordinates w ar model saleList=
    let
        readModelList =
                model.data
                    |> List.concat
                    |> List.map .value
                    |> List.Extra.transpose
        

        unit_priceFloats = List.map .unit_price saleList
        quantityFloats = List.map .quantity saleList
        taxFloats = List.map .tax saleList
        totalFloats = List.map .total saleList
        cogsFloats = List.map .cogs saleList
        gross_margin_percentageFloats = List.map .gross_margin_percentage saleList
        gross_incomeFloats = List.map .gross_income saleList

        scale1 =
            Scale.linear (h, 0) (wideExtent unit_priceFloats)
        scale2 =
            Scale.linear (h, 0) (wideExtent quantityFloats)
        scale3 =
            Scale.linear (h, 0) (wideExtent taxFloats)
        scale4 =
            Scale.linear (h, 0) (wideExtent totalFloats)
        scale5 =
            Scale.linear (h, 0) (wideExtent cogsFloats)
        scale6 =
            Scale.linear (h, 0) (wideExtent gross_margin_percentageFloats)
        scale7 =
            Scale.linear (h, 0) (wideExtent gross_incomeFloats)
        
        scaleX =
            Scale.linear (0, w) ( 1, 7)
        
        scaleList =
            [scale1, scale2, scale3, scale4, scale5, scale6, scale7]
        
        swapListItem l a b=
            List.Extra.swapAt a b l 


        listAxis =
            List.map (Axis.left [ Axis.tickCount 8 ]) (swapListItem scaleList indexA indexB)


    in
    svg [ viewBox 0 0 (w + 2 * padding) (h + 2 * padding), TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 
        [ style [] 
            []
        , g [ transform [ Translate (padding - 1) padding ] ] <|
                List.indexedMap
                    (\i axis ->
                        g
                            [ transform
                                [ Translate (Scale.convert scaleX (toFloat i + 1)) 0
                                ]
                            ]
                            [ axis ]
                    )
                    listAxis]
        ]
        
    
    

multiPoint : String -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> MultiDimPoint 
multiPoint pointName unit_price quantity tax total cogs gross_margin_percentage gross_income rating =
    MultiDimPoint pointName [unit_price, quantity, tax, total, cogs, gross_margin_percentage, gross_income, rating]

saleToMultiPoint: Sale -> MultiDimPoint
saleToMultiPoint sale =
    Maybe.map5 multiPoint (sale.unit_price) (sale.quantity) (sale.tax) (sale.total) (sale.cogs) (sale. gross_margin_percentage) (sale.gross_income) (sale.rating) 


type alias MultiDimPoint =
    { pointName : String, value : List Float }


type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }
