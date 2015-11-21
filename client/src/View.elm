module View where

import Html exposing (..)
import Types exposing (..)
import Signal exposing (Address)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element,container,middle)
import Color exposing (..)

boxToRect : Box -> Shape
boxToRect ((x1,y1), (x2,y2)) =
  rect (x2 - x1) (y2 - y1)

renderWall : Box -> Form
renderWall box =
  boxToRect box
  |> filled darkRed
  |> move (fst box)

renderPlayer : Player -> Form
renderPlayer player =
  boxToRect player.box
  |> filled darkBlue
  |> move player.position


renderScene : Scene -> Element
renderScene scene =
  let walls = List.map renderWall scene.walls
      players = List.map renderPlayer scene.players
      all = group (walls ++ players)
            |> scale 6
  in collage 800 300 [all]

root : Address Action -> Model -> Html
root address model =
  div []
      [h1 []
          [Html.text "Egg"]
      ,div []
           [code []
                 [Html.text <| toString model]]
      ,case model.scene of
        Just (Ok scene) -> fromElement (renderScene scene)
        Just (Err e) -> Html.text ("ERROR: " ++ e)
        Nothing -> Html.text "Waiting for data..."]
