module View where

import Html exposing (..)
import Types exposing (..)
import Signal exposing (Address)

root : Address Action -> Model -> Html
root address model =
  div []
      [h1 []
          [text "AEgg"]
      ,div []
           [code []
                 [text <| toString model]]]
