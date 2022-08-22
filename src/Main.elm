module Main exposing (..)

import Browser
import Html exposing (Html, text, pre)
import Http
import Csv.Decode exposing(Decoder, pipeline, string, float, column, int, into)
import Vis1 exposing (..)
import Data

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
