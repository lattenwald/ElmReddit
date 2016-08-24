module Styles exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (style)

subSubscribed : Attribute a
subSubscribed =
  style
    [ ("color", "green")
    , ("weight", "bold") ]

subNotSubscribed : Attribute a
subNotSubscribed =
  style
    [ ("color", "red") ]

list : Attribute a
list =
  style
    [ ("listStyle", "none")
    , ("padding", "0.5em") ]

item : Attribute a
item =
  style
    [ ("padding", "0.1em 0.5em")
    , ("cursor", "pointer") ]

focused : Attribute a
focused =
  style
    [ ("background", "#cfc") ]

empty : Attribute a
empty = style []
