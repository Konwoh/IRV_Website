
module Data exposing (..)

import Browser
import Html exposing (Html, text, pre)
import Http
import Csv.Decode exposing(Decoder, pipeline, string, float, column, int, into)
import Vis1 exposing (..)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Model
  = Failure
  | Loading
  | Success String


type Msg
  = GotText (Result Http.Error String)

type alias Sale =
  { invoice_ID : String
  , branch : Branch
  , city : City
  , customer_type : Customer_type
  , gender : Gender
  , product_line : Product_line
  , unit_price : Float
  , quantity : Int
  , tax : Float
  , total : Float
  , date : String
  , time : String
  , payment : Payment
  , cogs : Float
  , gross_margin_percentage : Float
  , gross_income : Float
  , rating : Float
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "https://cors-anywhere.herokuapp.com/https://cloud.informatik.uni-halle.de/s/pSKk3izyLRQ84C9/download/supermarket_sales%20-%20Sheet1.csv"
      , expect = Http.expectString GotText
      }
  )



view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    Loading ->
      text "Loading..."

    Success fullText ->
          let 
              salesData: List Sale
              salesData =  Result.withDefault [] (Csv.Decode.decodeCsv  Csv.Decode.FieldNamesFromFirstRow salesDecoder fullText)

              filteredSales = filterSales salesData
          in 
              Html.div []
                  [scatterplot filteredSales]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

salesDecoder : Decoder Sale
salesDecoder =
    into Sale
        |> pipeline (column 0 string)
        |> pipeline (column 1 string)
        |> pipeline (column 2 string)
        |> pipeline (column 3 string)
        |> pipeline (column 4 string)
        |> pipeline (column 5 string)
        |> pipeline (column 6 float)
        |> pipeline (column 7 int)
        |> pipeline (column 8 float)
        |> pipeline (column 9 float)
        |> pipeline (column 10 string)
        |> pipeline (column 11 string)
        |> pipeline (column 12 string)
        |> pipeline (column 13 float)
        |> pipeline (column 14 float)
        |> pipeline (column 15 float)
        |> pipeline (column 16 float)