module Main exposing (..)

import Browser
import Html exposing (Html, text)
import Http
import Csv.Decode as Decode
import Vis1 exposing (..)
import Data exposing(Sale, salesCopyDecoder, attributeFilter, attrToString, stringToAttr)
import Html exposing (div)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value)


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
    , attribute2: Data.Attributes}--}}

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
    , branch = Data.NoBranch
    , city = Data.NoCity
    , customer_type = Data.Normal
    , gender= Data.Male
    , product_line= Data.Electronic_accessories
    , payment= Data.Ewallet
    , selector = Data.Branch 
    , attribute1= Data.Unit_price
    , attribute2= Data.Quantity }
  , Http.get
      { url = "https://cors-anywhere.herokuapp.com/https://cloud.informatik.uni-halle.de/s/pSKk3izyLRQ84C9/download/supermarket_sales%20-%20Sheet1.csv"
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
            filteredBranchData = filterBranch data model.branch 
            filteredSalesData = filterSalesData (Data.selectorToStr model.selector) (Data.branchToStr model.branch) data
            xFloat = attributeFilter filteredBranchData model.attribute1
            yFloat = attributeFilter filteredBranchData model.attribute2

            pointsList: List Point
            pointsList = List.map3 Vis1.toPoint (List.map .invoice_ID data) (xFloat) (yFloat)

            xyData:XYdatapoint
            xyData = XYdatapoint (Data.attrToString model.attribute1) (Data.attrToString model.attribute2) pointsList
          
          in
          
          div [] [ buttonAttribut1
                 , buttonAttribut2
                 , buttonBranch
                 , buttonCity
                 , scatterplot xyData]

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
        [ onInput(\i -> SelectBranch (Data.strToSelector "Branch") (Data.stringToBranch i))]
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