
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
    | AllGender
    | NoGender

type Product_line
    = Health_and_beauty
    | Fashion_accessories
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

type Selector
    = Branch
    | City
    | Customer_type
    | Gender
    | Product_line
    | Payment
    | NoSelector

type AxisIndex 
    = Erste_Achse
    | Zweite_Achse
    | Dritte_Achse
    | Vierte_Achse
    | Fünfte_Achse
    | Sechste_Achse
    | Siebte_Achse
    | Achte_Achse

type IndexSelector
    = FirstIndex
    | SecondIndex


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
        "AllCity" ->
            AllCity
        _ -> 
            NoCity

decodeCustomerType: String -> Customer_type
decodeCustomerType str =
    case str of
        "Normal" ->
            Normal
        "Member" ->
            Member
        "AllCustomer" ->
            AllCustomer
        _ -> 
            NoCustomer

decodeGender: String -> Gender
decodeGender str =
    case str of
        "Male" ->
            Male
        "Female" ->
            Female
        "AllGender" ->
            AllGender
        _ ->
            NoGender

decodeProductLine: String -> Product_line
decodeProductLine str =
    case str of
       "Health and beauty" ->
            Health_and_beauty
       "Fashion accessories" ->
            Fashion_accessories
       "Electronic accessories" ->
            Electronic_accessories
       "Home and lifestyle" ->
            Home_and_lifestyle
       "Sports and travel" ->
            Sports_and_travel
       "Food and beverages" ->
            Food_and_beverages
       "AllProductLine" ->
            AllProductLine
       _ -> 
            NoProductLine

decodePayment: String -> Payment
decodePayment str =
    case str of
        "Ewallet" ->
            Ewallet
        "Cash" ->
            Cash
        "Credit.card" ->
            Credit_card
        "AllPayment" ->
            AllPayment
        _ ->
            NoPayment

attributeFilter: List Sale -> Attributes -> List (String, Float)
attributeFilter sales attr =
    case attr of
        Unit_price ->
            List.map (\i -> ("invoice_ID: " ++ i.invoice_ID, i.unit_price)) sales
        Quantity ->
            List.map (\i -> ("invoice_ID: " ++ i.invoice_ID, i.quantity)) sales
        Tax ->
            List.map (\i -> ("invoice_ID: " ++ i.invoice_ID, i.tax)) sales
        Total_price ->
            List.map (\i -> ("invoice_ID: " ++ i.invoice_ID, i.total)) sales
        Cogs ->
            List.map (\i -> ("invoice_ID: " ++ i.invoice_ID, i.cogs)) sales
        Gross_margin_percentage ->
            List.map (\i -> ("invoice_ID: " ++ i.invoice_ID, i.gross_margin_percentage)) sales
        Gross_income ->
            List.map (\i -> ("invoice_ID: " ++ i.invoice_ID, i.gross_income)) sales
        Rating ->
            List.map (\i -> ("invoice_ID: " ++ i.invoice_ID, i.rating)) sales
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

strToPayment2: String -> Payment
strToPayment2 str =
    case str of
        "Ewallet" ->
            Ewallet
        "Cash" ->
            Cash
        "Credit_card" ->
            Credit_card
        "AllPayment" ->
            AllPayment
        _ ->
            NoPayment


strToProductLine: String -> Product_line
strToProductLine str =
    case str of
       "Health and beauty" ->
            Health_and_beauty
       "Fashion accessories" ->
            Fashion_accessories
       "Electronic accessories" ->
            Electronic_accessories
       "Home and lifestyle" ->
            Home_and_lifestyle
       "Sports and travel" ->
            Sports_and_travel
       "Food and beverages" ->
            Food_and_beverages
       "AllProductLine" ->
            AllProductLine
       _ -> 
            NoProductLine

branchToStr : Branch -> String
branchToStr branch =
    case branch of
        A ->
            "A"
        B ->
            "B"
        C -> 
            "C"
        AllBranch ->
            "AllBranch"
        _ ->
            "Nichts ausgewählt"

cityToStr : City -> String
cityToStr city =
    case city of
        Yangon ->
            "Yangon"
        Naypyitaw ->
            "Naypyitaw"
        Mandalay ->
            "Mandalay"
        AllCity ->
            "AllCity"
        _ ->
            "None"
customerToStr : Customer_type -> String
customerToStr customer =
    case customer of
        Member ->
            "Member"
        Normal ->
            "Normal"
        AllCustomer ->
            "AllCustomer"
        _ ->
            "None"

genderToStr : Gender -> String
genderToStr gender =
    case gender of 
        Male ->
            "Male"
        Female ->
            "Female"
        AllGender ->
            "AllGender"
        _ ->
            "None"


productLineToStr : Product_line -> String
productLineToStr product =
    case product of
        Health_and_beauty ->
            "Health and beauty"
        Sports_and_travel ->
            "Sports and travel"
        Fashion_accessories ->
            "Fashion accessories"
        Electronic_accessories ->
            "Electronic accessories"
        Home_and_lifestyle ->
            "Home and lifestyle"
        Food_and_beverages ->
            "Food and beverages"
        AllProductLine ->
            "AllProductLine"
        _ ->
            "None"
paymentToStr : Payment -> String
paymentToStr payment =
    case payment of
        Ewallet ->
            "Ewallet"
        Cash ->
            "Cash"
        Credit_card ->
            "Credit_card"
        AllPayment ->
            "AllPayment"
        _ ->
            "None"

selectorToStr : Selector -> String
selectorToStr selector =
    case selector of
        Branch ->
            "Branch"
        City ->
            "City"
        Customer_type ->
            "Customer_type"
        Product_line ->
            "Product_line"
        Gender ->
            "Gender"
        Payment ->
            "Payment"
        NoSelector ->
            "NoSelector"

strToSelector : String -> Selector
strToSelector str =
    case str of
        "Branch" ->
            Branch
        "City" ->
            City
        "Customer_type" ->
            Customer_type
        "Product_line" ->
            Product_line
        "Gender" ->
            Gender
        "Payment" ->
            Payment
        _ ->
            NoSelector

indexSelectorToInt: AxisIndex -> Int
indexSelectorToInt idx =
    case idx of
        Erste_Achse ->
            0
        Zweite_Achse ->
            1
        Dritte_Achse ->
            2
        Vierte_Achse ->
            3
        Fünfte_Achse ->
            4
        Sechste_Achse ->
            5
        Siebte_Achse ->
            6
        Achte_Achse ->
            7


strToIndexSelector: String -> AxisIndex
strToIndexSelector str =
    case str of
        "Erste Achse" ->
            Erste_Achse
        "Zweite Achse" ->
            Zweite_Achse
        "Dritte Achse" ->
            Dritte_Achse
        "Vierte Achse" ->
            Vierte_Achse
        "Fünfte Achse" ->
            Fünfte_Achse
        "Sechste Achse" ->
            Sechste_Achse
        "Siebte Achse" ->
            Siebte_Achse
        _ ->
            Achte_Achse