module Data exposing (..)

import Collage exposing (Path)
import Keyboard

import Sprite exposing (Sprite)

type alias Model =
  { pacman : Sprite
  , score : Int
  , food : List Sprite
  , barriers : List Path
  }


type Msg
  = KeyMsg Keyboard.KeyCode
