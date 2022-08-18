module Main exposing (..)

main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

type Model
  = Failure
  | Loading
  | Success String

type Msg
  = GotText (Result Http.Error String)


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "https://cloud.informatik.uni-halle.de/s/pSKk3izyLRQ84C9/download/supermarket_sales%20-%20Sheet1.csv"
      , expect = Http.expectString GotText
      }
  )



view : Model -> Html.Html Msg
view model =
    Html.div [] []


update : Msg -> Model -> Model
update msg model =