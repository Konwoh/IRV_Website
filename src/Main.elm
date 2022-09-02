module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Csv.Decode as Decode
import Vis1 exposing (..)
import Vis2 exposing (..)
import Vis3 exposing (..)
import Data exposing(Sale, attributeFilter, attrToString, stringToAttr)
import Html exposing (div)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value, style)
import Csv.Decode exposing (string)
import Data exposing (selectorToStr)
import List.Extra
import List exposing (filter)
import Date exposing (..)
import Time exposing (Month(..), Weekday(..))
import Dict exposing (..)
import TypedSvg exposing (rect, svg)
import TypedSvg.Types exposing (Length(..), Paint(..))
import TypedSvg.Attributes exposing (x, strokeWidth, stroke)
import Scale
import Scale.Color
import Color
type LoadingState
  = Failure
  | Loading
  | Success 

type Msg
  = GotText (Result Http.Error String)
  | SelectBranch Data.Selector Data.Branch
  | SelectCity Data.Selector Data.City
  | SelectCustomerType Data.Selector Data.Customer_type
  | SelectGender Data.Selector Data.Gender
  | SelectProductLine Data.Selector Data.Product_line
  | SelectPayment Data.Selector Data.Payment
  | SelectAttribute Data.AttributeSelector Data.Attributes
  | SelectIndex Data.IndexSelector Data.AxisIndex
  | SelectPage Data.PageSelector
  | SelectBranch2 Data.Selector Data.Branch
  | SelectCity2 Data.Selector Data.City
  | SelectCustomerType2 Data.Selector Data.Customer_type
  | SelectGender2 Data.Selector Data.Gender
  | SelectProductLine2 Data.Selector Data.Product_line
  | SelectPayment2 Data.Selector Data.Payment 

type alias Model = 
    { loadingState: LoadingState
    , data: List Data.Sale
    , branch: Data.Branch
    , city: Data.City
    , customer_type: Data.Customer_type
    , gender: Data.Gender
    , product_line: Data.Product_line
    , payment: Data.Payment
    , selector: Data.Selector
    , selector2: Data.Selector
    , attribute1: Data.Attributes
    , attribute2: Data.Attributes
    , indexSelector1: Data.AxisIndex
    , indexSelector2: Data.AxisIndex
    , pageSelector: Data.PageSelector}

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( { loadingState = Loading
    , data = []
    , branch = Data.AllBranch
    , city = Data.AllCity
    , customer_type = Data.AllCustomer
    , gender= Data.AllGender
    , product_line= Data.AllProductLine
    , payment= Data.AllPayment
    , selector = Data.Branch
    , selector2 = Data.City 
    , attribute1= Data.Unit_price
    , attribute2= Data.Quantity
    , indexSelector1 = Data.Erste_Achse 
    , indexSelector2 = Data.Erste_Achse
    , pageSelector = Data.Scatterplot}
  , Http.get
      { url = "https://raw.githubusercontent.com/Konwoh/Information-Retrieveal/main/supermarket_neu2.csv"
      , expect = Http.expectString GotText
      }
  )

view : Model -> Html Msg
view model =
  case model.loadingState of
    Failure ->
      text "I was unable to load your book."

    Loading ->
      text "Loading..."

    Success ->
            div [style "background" "#80b3ff"] 
                [ div[style "border-color" "black", style "border-width" "3px", style "border-style" "solid" ] 
                     [ div [style "padding" "5px", style "font-size" "18px" ] 
                           [strong[][text "Grafik auswählen: "], buttonPlot]
                     , Html.br[][]
                     , div[style "display" "flex", style "font-size" "18px"] 
                         [ div [style "padding" "5px"][text "Branche auswählen: ", buttonBranch]
                         , div [style "padding" "5px"][text "Stadt auswählen: ", buttonCity]
                         , div [style "padding" "5px"][text "Kundenstatus auswählen: ", buttonCustomer]
                         , div [style "padding" "5px"][text "Produktlinie auswählen: ", buttonProduct]
                         , div [style "padding" "5px"][text "Geschlecht auswählen: ", buttonGender]
                         , div [style "padding" "5px"][text "Zahlungsart auswählen: ", buttonPayment]]
                         , div [style "display" "flex", style "font-size" "18px"] 
                                  [ div [style "padding" "5px"][text "Branche auswählen: ", buttonBranch2]
                                  , div [style "padding" "5px"][text "Stadt auswählen: ", buttonCity2]
                                  , div [style "padding" "5px"][text "Kundenstatus auswählen: ", buttonCustomer2]
                                  , div [style "padding" "5px"][text "Produktlinie auswählen: ", buttonProduct2]
                                  , div [style "padding" "5px"][text "Geschlecht auswählen: ", buttonGender2]
                                  , div [style "padding" "5px"][text "Zahlungsart auswählen: ", buttonPayment2]
                                  ]
                         
                     , Html.br [][]]
                
                   
                , div[ style "display" "flex", style "justify-content" "center", style "background" "#b3d1ff", style "height" "100vh" ][
                      div [ style "width" "1200px", style "padding" "60px", style "background" "white", style "overflow-x" "auto", style "border-color" "black", style "border-width" "3px", style "border-style" "solid", style "border-top-style" "none" ] [
          let
            data : List Data.Sale
            data = model.data
            --filteredBranchData = filterBranch data model.branch 
            nominalAttrSelector =
                if model.selector == Data.Branch then
                    Data.branchToStr model.branch
                else if model.selector == Data.City then
                    Data.cityToStr model.city
                else if model.selector == Data.Customer_type then
                    Data.customerToStr model.customer_type
                else if model.selector == Data.Product_line then
                    Data.productLineToStr model.product_line
                else if model.selector == Data.Gender then
                    Data.genderToStr model.gender
                else
                    Data.paymentToStr model.payment

            nominalAttrSelector2 =
                if model.selector2 == Data.Branch then
                    Data.branchToStr model.branch
                else if model.selector2 == Data.City then
                    Data.cityToStr model.city
                else if model.selector2 == Data.Customer_type then
                    Data.customerToStr model.customer_type
                else if model.selector2 == Data.Product_line then
                    Data.productLineToStr model.product_line
                else if model.selector2 == Data.Gender then
                    Data.genderToStr model.gender
                else
                    Data.paymentToStr model.payment
                

            filteredSalesData : List Data.Sale
            filteredSalesData = filterSalesData (Data.selectorToStr model.selector) (nominalAttrSelector) data

            filteredSalesData2: List Data.Sale
            filteredSalesData2= filterSalesData (Data.selectorToStr model.selector2) (nominalAttrSelector2) filteredSalesData
          
          in
          case model.pageSelector of
            Data.Scatterplot ->
              let
                -- x Werte der Koordinaten und Invoice ID als Tuple --
                xFloat : List (String, Float)
                xFloat = attributeFilter filteredSalesData2 model.attribute1
                -- y Werte der Koordinaten und Invoice ID als Tuple --
                yFloat : List (String, Float)
                yFloat = attributeFilter filteredSalesData2 model.attribute2
                -- Point Name wird durch Tuple.first rausgezogen und die x- und y-Koordinaten über Tuple.second --
                pointsList: List Point
                pointsList = List.map3 Vis1.toPoint (List.map (Tuple.first) xFloat) (List.map (Tuple.second) xFloat) (List.map (Tuple.second) yFloat)
                -- Daten werden hier in die XYdatapoint Datenstruktur geschrieben, um es der scatterplot Funktion zu übergeben --
                xyData: XYdatapoint
                xyData = XYdatapoint (Data.attrToString model.attribute1) (Data.attrToString model.attribute2) pointsList
              in 
                div[][ div[style "font-size" "20px"] [Html.text " Ausgewählte Dimension1: ", strong [] [Html.text (selectorToStr model.selector)]]
                     , div[style "font-size" "20px"] [Html.text " Ausgewähltes Attribut1: ", strong [] [Html.text (nominalAttrSelector)]]
                     , div[style "font-size" "20px"] [Html.text "Ausgewählte Dimension2: ", strong []  [Html.text (selectorToStr model.selector2)]]
                     , div[style "font-size" "20px"] [Html.text " Ausgewähltes Attribut2: ", strong [] [Html.text (nominalAttrSelector2)]]
                     , Html.br[][]
                     , div [style "padding" "5px"][text "x-Achse auswählen: ", buttonAttribut1]
                     , div [style "padding" "5px"] [text "y-Achse auswählen: ", buttonAttribut2]
                     , scatterplot xyData]
            Data.ParallelCoordPlot ->
              let
                  multiPoint : String -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> MultiDimPoint 
                  multiPoint pointName unit_price quantity tax total cogs gross_margin_percentage gross_income rating =
                      MultiDimPoint pointName (List.Extra.swapAt (Data.indexSelectorToInt model.indexSelector1) (Data.indexSelectorToInt model.indexSelector2) [unit_price, quantity, tax, total, cogs, gross_margin_percentage, gross_income, rating])

                  saleToMultiPoint: Data.Sale -> MultiDimPoint
                  saleToMultiPoint sale =
                      multiPoint (sale.invoice_ID) (sale.unit_price) (sale.quantity) (sale.tax) (sale.total) (sale.cogs) (sale.gross_margin_percentage) (sale.gross_income) (sale.rating) 

                  multiPointSale : List MultiDimPoint
                  multiPointSale = List.map saleToMultiPoint filteredSalesData2

                  multiDimData =
                    MultiDimData [ "Verkaufspreis", "Anzahl gekaufter Produkte", "5 % Steuer", "Gesamtpreis", "Kosten", "prozentuale Gewinnspanne", "Bruttoeinkommen", "Bewertung" ]
                      [multiPointSale]
              in
                div[][ div[style "font-size" "20px"] [Html.text " Ausgewählte Dimension1: ", strong [] [Html.text (selectorToStr model.selector)]]
                     , div[style "font-size" "20px"] [Html.text " Ausgewähltes Attribut1: ", strong [] [Html.text (nominalAttrSelector)]]
                     , div[style "font-size" "20px"] [Html.text "Ausgewählte Dimension2: ", strong []  [Html.text (selectorToStr model.selector2)]]
                     , div[style "font-size" "20px"] [Html.text " Ausgewähltes Attribut2: ", strong [] [Html.text (nominalAttrSelector2)]]
                     , Html.br[][]
                     , div [style "display" "flex"][ div [style "padding" "5px"][text "Tausche...: ", buttonIndex1]
                            , div [style "padding" "5px"][text "mit: ", buttonIndex2]]
                     , Vis2.parallelCoordinates 900 2 multiDimData data (Data.indexSelectorToInt model.indexSelector1) (Data.indexSelectorToInt model.indexSelector2)]

            Data.RecursivePatternPlot ->
              let
                  dateStringList =
                    List.map .date filteredSalesData2

                  totalList =
                    List.map .total filteredSalesData2
                      --|> List.map String.fromFloat 
                      --|> List.map String.toFloat

                  tupleStringList: List (String,Float)
                  tupleStringList = List.map2 Tuple.pair dateStringList totalList
                  tupleDateList: List(Date, Float)
                  tupleDateList = List.map (\(a, b) -> (Result.withDefault (fromOrdinalDate 1970 1)(Date.fromIsoString a), b)) tupleStringList

                  sortedTupleDateList =
                      List.sortWith (\t1 t2 -> Date.compare (Tuple.first t1) (Tuple.first t2)) tupleDateList

                  final =
                    List.map(\(a,b) -> (Date.toIsoString a,b)) sortedTupleDateList       

                  uniqueDates: List(String)
                  uniqueDates = List.map Tuple.first final
                                |> List.Extra.unique

                  dateSumList: List(String,Maybe Float)
                  dateSumList =
                    let
                        indexList: List(List(Int))
                        indexList = List.map(\a-> List.Extra.elemIndices a (List.map Tuple.first final) ) uniqueDates

                        valueList: List (List(Float))
                        valueList = List.map(\i-> List.map(\j-> Maybe.withDefault 0.0 (List.Extra.getAt j (List.map(\h -> (Tuple.second h)) final)))i ) indexList
                        sumList: List Float
                        sumList = List.map List.sum valueList

                    in
                        List.Extra.zip uniqueDates sumList
                          |> List.map(\(a,b) -> (a, String.fromFloat b))
                          |> List.map(\(a,b) -> (a, String.toFloat b)) 
                        
               in
                  div[][ div[style "font-size" "20px"] [Html.text " Ausgewählte Dimension1: ", strong [] [Html.text (selectorToStr model.selector)]]
                       , div[style "font-size" "20px"] [Html.text " Ausgewähltes Attribut1: ", strong [] [Html.text (nominalAttrSelector)]]
                       , div[style "font-size" "20px"] [Html.text "Ausgewählte Dimension2: ", strong []  [Html.text (selectorToStr model.selector2)]]
                       , div[style "font-size" "20px"] [Html.text " Ausgewähltes Attribut2: ", strong [] [Html.text (nominalAttrSelector2)]]                       
                       , div [ style "display" "flex", style "padding" "5px"]
                             [ text "Legende: "
                             , div [style "padding" "5px"] [colorContext 100 dateSumList, text "100"]
                             , div [style "padding" "5px"] [colorContext 200 dateSumList, text "200"]
                             , div [style "padding" "5px"] [colorContext 500 dateSumList, text "500"]
                             , div [style "padding" "5px"] [colorContext 1000 dateSumList, text "1000"]
                             , div [style "padding" "5px"] [colorContext 1500 dateSumList, text "1500"]
                             , div [style "padding" "5px"] [colorContext 2000 dateSumList, text "2000"]
                             , div [style "padding" "5px"] [colorContext 3000 dateSumList, text "3000"]
                             , div [style "padding" "5px"] [colorContext 4000 dateSumList, text "4000"]
                             , div [style "padding" "5px"] [colorContext 5000 dateSumList, text "5000"]
                             , div [style "padding" "5px"] [colorContext 6000 dateSumList, text "6000"]
                             , div [style "padding" "5px"] [colorContext 8000 dateSumList, text "8000"]
                             ]
                       , Vis3.recrusivePatternPlot dateSumList
                       , Html.br[][]
                       , div[style "font-size" "20px"] [strong [] [Html.text ("Dargestellte List: Datum und kumulierter Verkaufswert an diesem Tag")]]
                       , List.map
                            (\data2 ->
                                Html.li []
                                    [ Html.text
                                        (Tuple.first data2
                                            ++ ",  "
                                            ++ (Tuple.second data2
                                                    |> String.fromFloat
                                               )
                                        )
                                    ]
                            )
                            (List.map(\(a,b)-> (a, Maybe.withDefault 0.0 b)) dateSumList)
                            |> Html.ul []]
            ]]]

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          ({model | loadingState = Success, data = (Result.withDefault [] (Decode.decodeCsv  Decode.FieldNamesFromFirstRow Data.salesDecoder fullText))}, Cmd.none)
        Err _ ->
          ({model | loadingState = Failure}, Cmd.none)
    SelectBranch select branch ->
            ({model | branch = branch, selector = select}, Cmd.none)
    SelectCity select city ->
            ({model | city = city, selector = select}, Cmd.none)
    SelectCustomerType select customer_type ->
            ({model | customer_type = customer_type, selector = select}, Cmd.none)
    SelectGender select gender -> 
            ({model | gender = gender, selector = select}, Cmd.none)
    SelectProductLine select product_line -> 
            ({model | product_line = product_line, selector = select}, Cmd.none)
    SelectPayment select payment ->
            ({model | payment = payment, selector = select}, Cmd.none)
    SelectBranch2 select branch ->
            ({model | branch = branch, selector2 = select}, Cmd.none)
    SelectCity2 select city ->
            ({model | city = city, selector2 = select}, Cmd.none) 
    SelectCustomerType2 select customer_type ->
            ({model | customer_type = customer_type, selector2 = select}, Cmd.none)
    SelectGender2 select gender ->
            ({model | gender = gender, selector2 = select}, Cmd.none)
    SelectProductLine2 select product_line ->
            ({model | product_line = product_line, selector2 = select}, Cmd.none)
    SelectPayment2 select payment ->
            ({model | payment = payment, selector2 = select}, Cmd.none)
    SelectAttribute attributeSelector attribute ->
        case attributeSelector of
            Data.Attribute1 ->
                    ({model | attribute1 = attribute}, Cmd.none)
            Data.Attribute2 ->
                    ({model | attribute2 = attribute}, Cmd.none)
    SelectIndex indexSelector index ->
        case indexSelector of
            Data.FirstIndex ->
                    ({model | indexSelector1 = index}, Cmd.none)
            Data.SecondIndex ->
                    ({model | indexSelector2 = index}, Cmd.none)
    SelectPage page ->
          ({model | pageSelector = page}, Cmd.none)

buttonAttribut1 : Html Msg
buttonAttribut1 =
    Html.select
        [ onInput (\at -> SelectAttribute Data.Attribute1 (Data.stringToAttr at)) ]
        [ Html.option [ value "Unit price" ] [ Html.text "Unit price" ]
        , Html.option [ value "Quantity" ] [ Html.text "Quantity" ]
        , Html.option [ value "Tax" ] [ Html.text "Tax" ]
        , Html.option [ value "Total price" ] [ Html.text "Total price" ]
        , Html.option [ value "Cogs" ] [ Html.text "Cogs" ]
        , Html.option [ value "gross margin percentage" ] [ Html.text "gross margin percentage" ]
        , Html.option [ value "gross income" ] [ Html.text "gross income" ]
        , Html.option [ value "rating" ] [ Html.text "rating" ]
        ]

buttonAttribut2 : Html Msg
buttonAttribut2 =
    Html.select
        [ onInput (\i -> SelectAttribute Data.Attribute2 (Data.stringToAttr i)) ]
        [ Html.option [ value "Unit price" ] [ Html.text "Unit price" ]
        , Html.option [ value "Quantity" ] [ Html.text "Quantity" ]
        , Html.option [ value "Tax" ] [ Html.text "Tax" ]
        , Html.option [ value "Total price" ] [ Html.text "Totalprice" ]
        , Html.option [ value "Cogs" ] [ Html.text "Cogs" ]
        , Html.option [ value "gross margin percentage" ] [ Html.text "gross margin percentage" ]
        , Html.option [ value "gross income" ] [ Html.text "gross income" ]
        , Html.option [ value "rating" ] [ Html.text "rating" ]
        ]

buttonBranch : Html Msg
buttonBranch =
    Html.select 
        [ onInput(\i -> SelectBranch (Data.strToSelector "Branche") (Data.decodeBranch i))]
        [ Html.option [ value "A"] [Html.text "A"]
        , Html.option [ value "B"] [Html.text "B"]
        , Html.option [ value "C"] [Html.text "C"]
        , Html.option [ value "AllBranch"] [Html.text "Alle Branchen"]]


buttonCity : Html Msg
buttonCity =
    Html.select 
        [ onInput(\i -> SelectCity (Data.strToSelector "Stadt") (Data.decodeCity i))]
        [ Html.option [ value "Yangon"] [Html.text "Yangon"]
        , Html.option [ value "Naypyitaw"] [Html.text "Naypyitaw"]
        , Html.option [ value "Mandalay"] [Html.text "Mandalay"]
        , Html.option [ value "AllCity"] [Html.text "Alle Städte"]]

buttonCustomer : Html Msg
buttonCustomer =
    Html.select 
        [ onInput(\i -> SelectCustomerType (Data.strToSelector "Kundenart") (Data.decodeCustomerType i))]
        [ Html.option [ value "Member"] [Html.text "Mitglied"]
        , Html.option [ value "Normal"] [Html.text "Normaler Kunde"]
        , Html.option [ value "AllCustomer"] [Html.text "Alle Kunden"]]

buttonProduct : Html Msg
buttonProduct =
    Html.select 
        [ onInput(\i -> SelectProductLine (Data.strToSelector "Produktlinie") (Data.decodeProductLine i))]
        [ Html.option [ value "Health and beauty"] [Html.text "Health_and_beauty"]
        , Html.option [ value "Fashion accessories"] [Html.text "Fashion_accessories"]
        , Html.option [ value "Electronic accessories"] [Html.text "Electronic_accessories"]
        , Html.option [ value "Home and lifestyle"] [Html.text "Home_and_lifestyle"]
        , Html.option [ value "Sports and travel"] [Html.text "Sports_and_travel"]
        , Html.option [ value "Food and beverages"] [Html.text "Food_and_beverages"]
        , Html.option [ value "AllProductLine"] [Html.text "Alle Produktlinien"]]

buttonGender : Html Msg
buttonGender =
    Html.select 
        [ onInput(\i -> SelectGender (Data.strToSelector "Geschlecht") (Data.decodeGender i))]
        [ Html.option [ value "Male"] [Html.text "Mann"]
        , Html.option [ value "Female"] [Html.text "Frau"]
        , Html.option [ value "AllGender"] [Html.text "Alle Geschlechter"]]

buttonPayment : Html Msg
buttonPayment =
    Html.select 
        [ onInput(\i -> SelectPayment (Data.strToSelector "Zahlungsart") (Data.decodePayment i))]
        [ Html.option [ value "Ewallet"] [Html.text "E-Wallet"]
        , Html.option [ value "Cash"] [Html.text "Bargeld"]
        , Html.option [ value "Credit.card"] [Html.text "Kreditkarte"]
        , Html.option [ value "AllPayment"] [Html.text "Alle Zahlungsarten"]]


buttonIndex1 : Html Msg
buttonIndex1 =
    Html.select 
        [ onInput(\i -> SelectIndex Data.FirstIndex (Data.strToIndexSelector i))]
        [ Html.option [ value "Erste Achse"] [Html.text "Verkaufspreis"]
        , Html.option [ value "Zweite Achse"] [Html.text "Anzahl gekaufter Produkte"]
        , Html.option [ value "Dritte Achse"] [Html.text "5 % Steuer"]
        , Html.option [ value "Vierte Achse"] [Html.text "Gesamtpreis"]
        , Html.option [ value "Fünfte Achse"] [Html.text "Kosten"]
        , Html.option [ value "Sechste Achse"] [Html.text "prozentuale Gewinnspanne"]
        , Html.option [ value "Siebte Achse"] [Html.text "Bruttoeinkommen"]
        , Html.option [ value "Achte Achse"] [Html.text "Bewertung"]]

buttonIndex2 : Html Msg
buttonIndex2 =
    Html.select 
        [ onInput(\i -> SelectIndex Data.SecondIndex (Data.strToIndexSelector i))]
        [ Html.option [ value "Erste Achse"] [Html.text "Verkaufspreis"]
        , Html.option [ value "Zweite Achse"] [Html.text "Anzahl gekaufter Produkte"]
        , Html.option [ value "Dritte Achse"] [Html.text "5 % Steuer"]
        , Html.option [ value "Vierte Achse"] [Html.text "Gesamtpreis"]
        , Html.option [ value "Fünfte Achse"] [Html.text "Kosten"]
        , Html.option [ value "Sechste Achse"] [Html.text "prozentuale Gewinnspanne"]
        , Html.option [ value "Siebte Achse"] [Html.text "Bruttoeinkommen"]
        , Html.option [ value "Achte Achse"] [Html.text "Bewertung"]]

buttonPlot : Html Msg
buttonPlot =
    Html.select
        [ onInput(\i -> SelectPage (Data.strToPageSelector i))]
        [ Html.option [ value "Scatterplot"] [Html.text "Scatterplot"]
        , Html.option [ value "ParallelCoordPlot"] [Html.text "ParallelCoordsPlot"]
        , Html.option [ value "RecursivePatternPlot"] [Html.text "Recursive Pattern Plot"]
        ]

buttonBranch2 : Html Msg
buttonBranch2 =
    Html.select 
        [ onInput(\i -> SelectBranch2 (Data.strToSelector "Branche") (Data.decodeBranch i))]
        [ Html.option [ value "A"] [Html.text "A"]
        , Html.option [ value "B"] [Html.text "B"]
        , Html.option [ value "C"] [Html.text "C"]
        , Html.option [ value "AllBranch"] [Html.text "Alle Branchen"]]


buttonCity2 : Html Msg
buttonCity2 =
    Html.select 
        [ onInput(\i -> SelectCity2 (Data.strToSelector "Stadt") (Data.decodeCity i))]
        [ Html.option [ value "Yangon"] [Html.text "Yangon"]
        , Html.option [ value "Naypyitaw"] [Html.text "Naypyitaw"]
        , Html.option [ value "Mandalay"] [Html.text "Mandalay"]
        , Html.option [ value "AllCity"] [Html.text "Alle Städte"]]

buttonCustomer2 : Html Msg
buttonCustomer2 =
    Html.select 
        [ onInput(\i -> SelectCustomerType2 (Data.strToSelector "Kundenart") (Data.decodeCustomerType i))]
        [ Html.option [ value "Member"] [Html.text "Mitglied"]
        , Html.option [ value "Normal"] [Html.text "Normaler Kunde"]
        , Html.option [ value "AllCustomer"] [Html.text "Alle Kunden"]]

buttonProduct2 : Html Msg
buttonProduct2 =
    Html.select 
        [ onInput(\i -> SelectProductLine2 (Data.strToSelector "Produktlinie") (Data.decodeProductLine i))]
        [ Html.option [ value "Health and beauty"] [Html.text "Health_and_beauty"]
        , Html.option [ value "Fashion accessories"] [Html.text "Fashion_accessories"]
        , Html.option [ value "Electronic accessories"] [Html.text "Electronic_accessories"]
        , Html.option [ value "Home and lifestyle"] [Html.text "Home_and_lifestyle"]
        , Html.option [ value "Sports and travel"] [Html.text "Sports_and_travel"]
        , Html.option [ value "Food and beverages"] [Html.text "Food_and_beverages"]
        , Html.option [ value "AllProductLine"] [Html.text "Alle Produktlinien"]]

buttonGender2 : Html Msg
buttonGender2 =
    Html.select 
        [ onInput(\i -> SelectGender2 (Data.strToSelector "Geschlecht") (Data.decodeGender i))]
        [ Html.option [ value "Male"] [Html.text "Mann"]
        , Html.option [ value "Female"] [Html.text "Frau"]
        , Html.option [ value "AllGender"] [Html.text "Alle Geschlechter"]]

buttonPayment2 : Html Msg
buttonPayment2 =
    Html.select 
        [ onInput(\i -> SelectPayment2 (Data.strToSelector "Zahlungsart") (Data.decodePayment i))]
        [ Html.option [ value "Ewallet"] [Html.text "E-Wallet"]
        , Html.option [ value "Cash"] [Html.text "Bargeld"]
        , Html.option [ value "Credit.card"] [Html.text "Kreditkarte"]
        , Html.option [ value "AllPayment"] [Html.text "Alle Zahlungsarten"]]