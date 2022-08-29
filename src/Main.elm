module Main exposing (..)

import Browser
import Html exposing (Html, text)
import Http
import Csv.Decode as Decode
import Vis1 exposing (..)
import Vis2 exposing (..)
import Data exposing(Sale, salesCopyDecoder, attributeFilter, attrToString, stringToAttr)
import Html exposing (div)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value)
import Csv.Decode exposing (string)
import Data exposing (selectorToStr)
import List.Extra


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
    , attribute1: Data.Attributes
    , attribute2: Data.Attributes
    , indexSelector1: Data.AxisIndex
    , indexSelector2: Data.AxisIndex}--}}

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
    , city = Data.NoCity
    , customer_type = Data.Normal
    , gender= Data.Male
    , product_line= Data.Health_and_beauty
    , payment= Data.Ewallet
    , selector = Data.Branch 
    , attribute1= Data.Unit_price
    , attribute2= Data.Quantity
    , indexSelector1 = Data.Erste_Achse 
    , indexSelector2 = Data.Erste_Achse}
  , Http.get
      { url = "https://raw.githubusercontent.com/Konwoh/Information-Retrieveal/main/supermarket_sales.csv"
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
          let
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
                
            invoiceData = List.map .invoice_ID data
            filteredSalesData = filterSalesData (Data.selectorToStr model.selector) (nominalAttrSelector) data

            -- x Werte der Koordinaten und Invoice ID als Tuple --
            xFloat : List (String, Float)
            xFloat = attributeFilter filteredSalesData model.attribute1
            -- y Werte der Koordinaten und Invoice ID als Tuple --
            yFloat : List (String, Float)
            yFloat = attributeFilter filteredSalesData model.attribute2
            -- Point Name wird durch Tuple.first rausgezogen und die x- und y-Koordinaten über Tuple.second --
            pointsList: List Point
            pointsList = List.map3 Vis1.toPoint (List.map (Tuple.first) xFloat) (List.map (Tuple.second) xFloat) (List.map (Tuple.second) yFloat)
            -- Daten werden hier in die XYdatapoint Datenstruktur geschrieben, um es der scatterplot Funktion zu übergeben --
            xyData:XYdatapoint
            xyData = XYdatapoint (Data.attrToString model.attribute1) (Data.attrToString model.attribute2) pointsList

            multiPointSale : List MultiDimPoint
            multiPointSale = List.map Vis2.saleToMultiPoint data

            multiDimData =
              MultiDimData [ "unit_price", "quantity", "tax", "total", "cogs", "gross_margin_percentage", "gross_income", "rating" ]
                [multiPointSale]
          
          in
          
          div [] [ buttonAttribut1
                 , buttonAttribut2
                 , buttonBranch
                 , buttonCity
                 , buttonCustomer
                 , buttonProduct
                 , buttonGender
                 , buttonPayment
                 , Html.br [][]
                 , Html.text "Angwendeter Filter:"
                 , Html.text (" " ++ (selectorToStr model.selector))
                 , Html.text (" " ++ nominalAttrSelector)
                 , scatterplot xyData
                 , Vis2.parallelCoordinates 900 2 multiDimData data
                 ]

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
    SelectAttribute attributeSelector attribute ->
        case attributeSelector of
            Data.Attribute1 ->
                    ({model | attribute1 = attribute}, Cmd.none)
            Data.Attribute2 ->
                    ({model | attribute2 = attribute}, Cmd.none)

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
        [ onInput(\i -> SelectBranch (Data.strToSelector "Branch") (Data.decodeBranch i))]
        [ Html.option [ value "A"] [Html.text "A"]
        , Html.option [ value "B"] [Html.text "B"]
        , Html.option [ value "C"] [Html.text "C"]
        , Html.option [ value "AllBranch"] [Html.text "All Branches"]]


buttonCity : Html Msg
buttonCity =
    Html.select 
        [ onInput(\i -> SelectCity (Data.strToSelector "City") (Data.decodeCity i))]
        [ Html.option [ value "Yangon"] [Html.text "Yangon"]
        , Html.option [ value "Naypyitaw"] [Html.text "Naypyitaw"]
        , Html.option [ value "Mandalay"] [Html.text "Mandalay"]
        , Html.option [ value "AllCity"] [Html.text "AllCity"]]

buttonCustomer : Html Msg
buttonCustomer =
    Html.select 
        [ onInput(\i -> SelectCustomerType (Data.strToSelector "Customer_type") (Data.decodeCustomerType i))]
        [ Html.option [ value "Member"] [Html.text "Member"]
        , Html.option [ value "Normal"] [Html.text "Normal"]
        , Html.option [ value "AllCustomer"] [Html.text "AllCustomer"]]

buttonProduct : Html Msg
buttonProduct =
    Html.select 
        [ onInput(\i -> SelectProductLine (Data.strToSelector "Product_line") (Data.decodeProductLine i))]
        [ Html.option [ value "Health and beauty"] [Html.text "Health_and_beauty"]
        , Html.option [ value "Fashion accessories"] [Html.text "Fashion_accessories"]
        , Html.option [ value "Electronic accessories"] [Html.text "Electronic_accessories"]
        , Html.option [ value "Home and lifestyle"] [Html.text "Home_and_lifestyle"]
        , Html.option [ value "Sports and travel"] [Html.text "Sports_and_travel"]
        , Html.option [ value "Food and beverages"] [Html.text "Food_and_beverages"]
        , Html.option [ value "AllProductLine"] [Html.text "AllProductLine"]]

buttonGender : Html Msg
buttonGender =
    Html.select 
        [ onInput(\i -> SelectGender (Data.strToSelector "Gender") (Data.decodeGender i))]
        [ Html.option [ value "Male"] [Html.text "Male"]
        , Html.option [ value "Female"] [Html.text "Female"]
        , Html.option [ value "AllGender"] [Html.text "AllGender"]]

buttonPayment : Html Msg
buttonPayment =
    Html.select 
        [ onInput(\i -> SelectPayment (Data.strToSelector "Payment") (Data.strToPayment2 i))]
        [ Html.option [ value "Ewallet"] [Html.text "Ewallet"]
        , Html.option [ value "Cash"] [Html.text "Cash"]
        , Html.option [ value "Credit_card"] [Html.text "Credit_card"]
        , Html.option [ value "AllPayment"] [Html.text "AllPayment"]]