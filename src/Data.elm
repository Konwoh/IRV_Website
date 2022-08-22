
module Data exposing (..)

import Browser
import Html exposing (Html, text, pre)
import Http
import Csv.Decode exposing(Decoder, pipeline, string, float, column, int, into)



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
  , branch : String
  , city : String
  , customer_type : String
  , gender : String
  , product_line : String
  , unit_price : Float
  , quantity : Int
  , tax : Float
  , total : Float
  , date : String
  , time : String
  , payment : String
  , cogs : Float
  , gross_margin_percentage : Float
  , gross_income : Float
  , rating : Float
  }

type Branch
    = A
    | B
    | C


type City
    = Yangon
    | Naypyitaw
    | Mandalay

type Customer_type
    = Normal
    | Member

type Gender 
    = Male
    | Female

type Product_line
    = Health_and_beauty
    | Electronic_accessories
    | Home_and_lifestyle
    | Sports_and_travel
    | Food_and_beverages

type Payment 
    = Ewallet
    | Cash
    | Credit_card

type Attributes
    = Unit_pric
    | Quantity
    | Tax
    | Total_price
    | Cogs
    | Gross_margin_percentage
    | Gross_income
    | Rating

type AttributeSelector
    = Attribute1
    | Attribute2

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

          in 
              List.map
                (\data ->
                    Html.li []
                        [ Html.text data.invoice_ID]
                )
                salesData
                |> Html.ul []

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
    Decode.into Sale
        |> Decode.pipeline (Decode.field "Invoice_ID" Decode.string)
        |> Decode.pipeline (Decode.field "Branch" (Decode.map (\i -> case i of  "A" -> A 
                                                                                "B" -> B
                                                                                "C" -> C 
                                                                                _ -> Nope) Decode.string))
        |> Decode.pipeline (Decode.field "City" Decode.string)
        |> Decode.pipeline (Decode.field "Customer Type" Decode.string)
        |> Decode.pipeline (Decode.field "Gender" Decode.string)
        |> Decode.pipeline (Decode.field "Product line" Decode.string)
        |> Decode.pipeline (Decode.field "Unit price" Decode.float)
        |> Decode.pipeline (Decode.field "quantity" Decode.int)
        |> Decode.pipeline (Decode.field "tax" Decode.float)
        |> Decode.pipeline (Decode.field "Total" Decode.float)
        |> Decode.pipeline (Decode.field "date" Decode.string)
        |> Decode.pipeline (Decode.field "time" Decode.string)
        |> Decode.pipeline (Decode.field "payment" Decode.string)
        |> Decode.pipeline (Decode.field "cogs" Decode.float)
        |> Decode.pipeline (Decode.field "gross margin percentage" Decode.float)
        |> Decode.pipeline (Decode.field "gross income" Decode.float)
        |> Decode.pipeline (Decode.field "rating" Decode.float)