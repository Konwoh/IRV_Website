module Main exposing (..)

main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

view : Model -> Html.Html Msg
view model =
    Html.div [] []


update : Msg -> Model -> Model
update msg model =