module Types where

import Json.Decode exposing (..)

type alias Box = (Position, Position)

type alias Position = (Float, Float)

type alias Wall =
  {position : Position
  ,box : Box}

type alias Player =
  {name : String
  ,position : Position
  ,box : Box}

type alias Egg =
  {position : Position
  ,box : Box}

type alias Explosion =
  {position : Position
  ,box : Box}

type alias Scene =
  {walls : List Wall
  ,players : List Player
  ,eggs : List Egg
  ,explosions : List Explosion
  ,scores : List (String,Int)}

type alias Model =
  {scene : Maybe (Result String Scene)}

type Action
  = NewScene (Result String Scene)

decodePosition : Decoder Position
decodePosition = tuple2 (,) float float

decodeBox : Decoder Box
decodeBox = tuple2 (,) decodePosition decodePosition

decodeWall : Decoder Wall
decodeWall =
  object2 Wall
    ("pos" := decodePosition)
    ("bounds" := decodeBox)

decodePlayer : Decoder Player
decodePlayer =
  object3 Player
    ("name" := string)
    ("pos" := decodePosition)
    ("bounds" := decodeBox)

decodeEgg : Decoder Egg
decodeEgg =
  object2 Egg
    ("pos" := decodePosition)
    ("bounds" := decodeBox)

decodeExplosion : Decoder Explosion
decodeExplosion =
  object2 Explosion
    ("pos" := decodePosition)
    ("bounds" := decodeBox)

decodeScore : Decoder (String,Int)
decodeScore =
  object2 (,)
    ("name" := string)
    ("points" := int)

decodeScene : Decoder Scene
decodeScene =
  object5 Scene
    ("walls" := list decodeWall)
    ("players" := list decodePlayer)
    ("eggs" := list decodeEgg)
    ("explosions" := list decodeExplosion)
    ("scores" := list decodeScore)
