
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
  , quantity : Float
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
  , quantity : Float
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
    | AllBranch
    | NoBranch


type City
    = Yangon
    | Naypyitaw
    | Mandalay
    | AllCity
    | NoCity

type Customer_type
    = Normal
    | Member
    | AllCustomer
    | NoCustomer

type Gender 
    = Male
    | Female
    | AllGenderA
    | NoGender

type Product_line
    = Health_and_beauty
    | Electronic_accessories
    | Home_and_lifestyle
    | Sports_and_travel
    | Food_and_beverages
    | AllProductLine
    | NoProductLine

type Payment 
    = Ewallet
    | Cash
    | Credit_card
    | AllPayment
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
    | NoAttribute

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
        |> Decode.pipeline (Decode.column 7  Decode.float)
        |> Decode.pipeline (Decode.column 8  Decode.float)
        |> Decode.pipeline (Decode.column 9  Decode.float)
        |> Decode.pipeline (Decode.column 10 Decode.string)
        |> Decode.pipeline (Decode.column 11 Decode.string)
        |> Decode.pipeline (Decode.column 12 Decode.string)
        |> Decode.pipeline (Decode.column 13 Decode.float)
        |> Decode.pipeline (Decode.column 14 Decode.float)
        |> Decode.pipeline (Decode.column 15 Decode.float)
        |> Decode.pipeline (Decode.column 16 Decode.float)


salesDecoder : Decoder Sale
salesDecoder =
    Decode.into Sale
        |> Decode.pipeline (Decode.column 0  Decode.string)
        |> Decode.pipeline (Decode.column 1  (Decode.map decodeBranch Decode.string))
        |> Decode.pipeline (Decode.column 2  (Decode.map decodeCity Decode.string))
        |> Decode.pipeline (Decode.column 3  (Decode.map decodeCustomerType Decode.string))
        |> Decode.pipeline (Decode.column 4  (Decode.map decodeGender Decode.string))
        |> Decode.pipeline (Decode.column 5  (Decode.map decodeProductLine Decode.string))
        |> Decode.pipeline (Decode.column 6  Decode.float)
        |> Decode.pipeline (Decode.column 7  Decode.float)
        |> Decode.pipeline (Decode.column 8  Decode.float)
        |> Decode.pipeline (Decode.column 9  Decode.float)
        |> Decode.pipeline (Decode.column 10 Decode.string)
        |> Decode.pipeline (Decode.column 11 Decode.string)
        |> Decode.pipeline (Decode.column 12 (Decode.map decodePayment Decode.string))
        |> Decode.pipeline (Decode.column 13 Decode.float)
        |> Decode.pipeline (Decode.column 14 Decode.float)
        |> Decode.pipeline (Decode.column 15 Decode.float)
        |> Decode.pipeline (Decode.column 16 Decode.float)


decodeBranch: String -> Branch
decodeBranch str =
    case str of
        "A" ->
            A
        "B" ->
            B
        "C" ->
            C
        "AllBranch" ->
            AllBranch
        _ ->
            NoBranch

decodeCity: String -> City
decodeCity str = 
    case str of 
        "Yangon" ->
            Yangon
        "Naypyitaw" -> 
            Naypyitaw
        "Mandalay" -> 
            Mandalay
        _ -> 
            NoCity

decodeCustomerType: String -> Customer_type
decodeCustomerType str =
    case str of
        "Normal" ->
            Normal
        "Member" ->
            Member
        _ -> 
            NoCustomer

decodeGender: String -> Gender
decodeGender str =
    case str of
        "Male" ->
            Male
        "Female" ->
            Female
        _ ->
            NoGender

decodeProductLine: String -> Product_line
decodeProductLine str =
    case str of
       "Health and beauty" ->
            Health_and_beauty
       "Electronic accessories" ->
            Electronic_accessories
       "Home and lifestyle" ->
            Home_and_lifestyle
       "Sports and travel" ->
            Sports_and_travel
       "Food and beverages" ->
            Food_and_beverages
       _ -> 
            NoProductLine

decodePayment: String -> Payment
decodePayment str =
    case str of
        "Ewallet" ->
            Ewallet
        "Cash" ->
            Cash
        "Credit card" ->
            Credit_card
        _ ->
            NoPayment

attributeFilter: List Sale -> Attributes -> List Float
attributeFilter sales attr =
    case attr of
        Unit_price ->
            List.map .unit_price sales
        Quantity ->
            List.map .quantity sales
        Tax ->
            List.map .tax sales
        Total_price ->
            List.map .total sales
        Cogs ->
            List.map .cogs sales
        Gross_margin_percentage ->
            List.map .gross_margin_percentage sales
        Gross_income ->
            List.map .gross_income sales
        Rating ->
            List.map .rating sales
        NoAttribute ->
            []

attrToString: Attributes -> String
attrToString attr =
    case attr of 
        Unit_price ->
            "Unit price"
        Quantity ->
            "Quantity"
        Tax ->
            "Tax"
        Total_price ->
            "Total price"
        Cogs ->
            "cogs"
        Gross_margin_percentage ->
            "gross margin percentage"
        Gross_income ->
            "gross income"
        Rating ->
            "rating"
        NoAttribute ->
            "NoAttribute"

stringToAttr: String -> Attributes
stringToAttr str =
    case str of
        "Unit price" ->
            Unit_price
        "Quantity" ->
            Quantity
        "Tax" ->
            Tax
        "Total price" ->
            Total_price
        "Cogs" ->
            Cogs
        "gross margin percentage" ->
            Gross_margin_percentage
        "gross income" ->
            Gross_income
        "rating" ->
            Rating
        _ ->
            NoAttribute

stringToBranch str =
    case str of
        "A" ->
            A
        "B" ->
            B
        "C" ->
            C
        "AllBranch" ->
            AllBranch
        _ ->
            NoBranch
