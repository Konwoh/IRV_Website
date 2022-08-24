
module Data exposing (..)

import Csv.Decode as Decode exposing (Decoder)

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

type alias SaleCopy =
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
    | None


type City
    = Yangon
    | Naypyitaw
    | Mandalay
    | NoCity

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
    | NoProductLine

type Payment 
    = Ewallet
    | Cash
    | Credit_card
    | NoPayment

type Attributes
    = Unit_price
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

salesCopyDecoder : Decoder SaleCopy
salesCopyDecoder =
    Decode.into SaleCopy
        |> Decode.pipeline (Decode.column 0  Decode.string)
        |> Decode.pipeline (Decode.column 1  Decode.string)
        |> Decode.pipeline (Decode.column 2  Decode.string)
        |> Decode.pipeline (Decode.column 3  Decode.string)
        |> Decode.pipeline (Decode.column 4  Decode.string)
        |> Decode.pipeline (Decode.column 5  Decode.string)
        |> Decode.pipeline (Decode.column 6  Decode.float)
        |> Decode.pipeline (Decode.column 7  Decode.int)
        |> Decode.pipeline (Decode.column 8  Decode.float)
        |> Decode.pipeline (Decode.column 9  Decode.float)
        |> Decode.pipeline (Decode.column 10 Decode.string)
        |> Decode.pipeline (Decode.column 11 Decode.string)
        |> Decode.pipeline (Decode.column 12 Decode.string)
        |> Decode.pipeline (Decode.column 13 Decode.float)
        |> Decode.pipeline (Decode.column 14 Decode.float)
        |> Decode.pipeline (Decode.column 15 Decode.float)
        |> Decode.pipeline (Decode.column 16 Decode.float)