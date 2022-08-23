
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

salesDecoder : Decoder Sale
salesDecoder =
    Decode.into Sale
        |> Decode.pipeline (Decode.field "Invoice_ID" Decode.string)
        |> Decode.pipeline (Decode.field "Branch" (Decode.map (\i -> case i of "A" -> A 
                                                                               "B" -> B
                                                                               "C" -> C 
                                                                               _ -> None) Decode.string))
        |> Decode.pipeline (Decode.field "City" (Decode.map (\i -> case i of "Yangon" -> Yangon
                                                                             "Naypyitaw" -> Naypyitaw 
                                                                             "Mandalay" -> Mandalay 
                                                                             _ -> NoCity) Decode.string))
        |> Decode.pipeline (Decode.field "Customer Type" (Decode.map (\i -> if i == "Normal" then Normal else Member) Decode.string))
        |> Decode.pipeline (Decode.field "Gender" (Decode.map (\i -> if i == "Male" then Male else Female) Decode.string))
        |> Decode.pipeline (Decode.field "Product line" (Decode.map (\i -> case i of "Health_and_beauty" -> Health_and_beauty 
                                                                                     "Electronic_accessories" -> Electronic_accessories 
                                                                                     "Home_and_lifestyle" -> Home_and_lifestyle 
                                                                                     "Sports_and_travel" -> Sports_and_travel 
                                                                                     "Food_and_beverages" -> Food_and_beverages 
                                                                                     _ -> NoProductLine) Decode.string))
        |> Decode.pipeline (Decode.field "Unit price" Decode.float)
        |> Decode.pipeline (Decode.field "quantity" Decode.int)
        |> Decode.pipeline (Decode.field "tax" Decode.float)
        |> Decode.pipeline (Decode.field "Total" Decode.float)
        |> Decode.pipeline (Decode.field "date" Decode.string)
        |> Decode.pipeline (Decode.field "time" Decode.string)
        |> Decode.pipeline (Decode.field "payment" (Decode.map (\i -> case i of "Ewallet" -> Ewallet 
                                                                                "Cash" -> Cash 
                                                                                "Credit_card" -> Credit_card 
                                                                                _ -> NoPayment) Decode.string))
        |> Decode.pipeline (Decode.field "cogs" Decode.float)
        |> Decode.pipeline (Decode.field "gross margin percentage" Decode.float)
        |> Decode.pipeline (Decode.field "gross income" Decode.float)
        |> Decode.pipeline (Decode.field "rating" Decode.float)