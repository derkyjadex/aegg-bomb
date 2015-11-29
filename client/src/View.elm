module View where

import Html exposing (..)
import Types exposing (..)
import Signal exposing (Address)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element,container,middle)
import Color exposing (..)

boxToRect : Box -> Shape
boxToRect ((minX,minY), (maxX,maxY)) =
  rect (maxX - minX) (maxY - minY)

moveRect : Box -> Form -> Form
moveRect ((minX, minY), (maxX, maxY)) =
  let w = maxX - minX
      h = maxY - minY
      x = (w / 2) + minX
      y = (h / 2) + minY
  in move (x, y)

renderWall : Wall -> Form
renderWall wall =
  boxToRect wall.box
  |> filled darkRed
  |> moveRect wall.box
  |> move wall.position

renderPlayer : Player -> Form
renderPlayer player =
  boxToRect player.box
  |> filled darkBlue
  |> moveRect player.box
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
