module Types where

import Json.Decode exposing (..)

type alias Box = (Position, Position)

type alias Position = (Float, Float)

type alias Player =
  {name : String
  ,position : Position
  ,box : Box}

type alias Scene =
  {walls : List Box
  ,players : List Player}

type alias Model =
  {scene : Maybe (Result String Scene)}

type Action
  = NewScene (Result String Scene)

decodePosition : Decoder Position
decodePosition = tuple2 (,) float float

decodeBox : Decoder Box
decodeBox = tuple2 (,) decodePosition decodePosition

decodePlayer : Decoder Player
decodePlayer =
  tuple3 Player string decodePosition decodeBox

decodeScene : Decoder Scene
decodeScene =
  object2 Scene
    ("_walls" := list decodeBox)
    ("_players" := list decodePlayer)
