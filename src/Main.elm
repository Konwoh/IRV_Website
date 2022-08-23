module Main exposing (..)

import Browser
import Html exposing (Html, text)
import Http
import Csv.Decode as Decode
import Vis1 exposing (..)
import Data exposing(Sale, salesDecoder)
import Html exposing (div)


type LoadingState
  = Failure
  | Loading
  | Success 

type Msg
  = GotText (Result Http.Error String)
  | SelectBranch Data.Branch
  | SelectCity Data.City
  | SelectCustomerType Data.Customer_type
  | SelectGender Data.Gender
  | SelectProductLine Data.Product_line
  | SelectPayment Data.Payment
  | SelectAttribute Data.AttributeSelector Data.Attributes

type alias Model = 
    { loadingState: LoadingState
    , data: List Sale
    , branch: Data.Branch
    , city: Data.City
    , customer_type: Data.Customer_type
    , gender: Data.Gender
    , product_line: Data.Product_line
    , payment: Data.Payment
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
    , branch = Data.A
    , city = Data.Naypyitaw
    , customer_type = Data.Normal
    , gender= Data.Male
    , product_line= Data.Electronic_accessories
    , payment= Data.Ewallet
    , attribute1= Data.Quantity
    , attribute2= Data.Rating }
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
          div [] [scatterplot (filterSales model.data)]

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          ({model | loadingState = Success, data = (Result.withDefault [] (Decode.decodeCsv  Decode.FieldNamesFromFirstRow salesDecoder fullText))}, Cmd.none)

        Err _ ->
          ({model | loadingState = Failure}, Cmd.none)
    SelectBranch branch ->
            ({model | branch = branch}, Cmd.none)
    SelectCity city ->
            ({model | city = city}, Cmd.none)
    SelectCustomerType customer_type ->
            ({model | customer_type = customer_type}, Cmd.none)
    SelectGender gender -> 
            ({model | gender = gender}, Cmd.none)
    SelectProductLine product_line -> 
            ({model | product_line = product_line}, Cmd.none)
    SelectPayment payment ->
            ({model | payment = payment}, Cmd.none)
    SelectAttribute attributeSelector attribute ->
        case attributeSelector of
            Data.Attribute1 ->
                    ({model | attribute1 = attribute}, Cmd.none)
            Data.Attribute2 ->
                    ({model | attribute2 = attribute}, Cmd.none)

