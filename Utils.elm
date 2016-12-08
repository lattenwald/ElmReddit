module Utils exposing (..)

import Task
import Json.Decode exposing (Decoder, at)
import Dict

unique : List comparable -> List comparable
unique l =
  let
    folder item acc =
      case acc of
        [] ->
          [ item ]

        head :: rest ->
          if item == head then
            acc
          else
            item :: acc
  in
    l
      |> List.sort
      |> List.foldl folder []
      |> List.reverse


diff : List comparable -> List comparable -> List comparable
diff a b =
  let
    a_sorted =
      List.sort a

    b_sorted =
      List.sort b

    diff_ a b acc =
      case ( a, b ) of
        ( [], _ ) ->
          acc

        ( _, [] ) ->
          a ++ acc

        ( a_ :: arest, b_ :: brest ) ->
          case compare a_ b_ of
            EQ ->
              diff_ arest brest acc

            LT ->
              diff_ arest b (a_ :: acc)

            GT ->
              diff_ a brest acc
  in
    diff_ a_sorted b_sorted []


ignore : ignored -> a -> a
ignore _ a =
  a


maybeToList : Maybe a -> List a
maybeToList a =
  case a of
    Nothing ->
      []

    Just a ->
      [ a ]


fire : a -> Cmd a
fire msg =
  Task.perform identity (Task.succeed msg)


fromJsonUnit : a -> Json.Decode.Decoder a
fromJsonUnit val =
  Json.Decode.dict Json.Decode.string |> Json.Decode.andThen
    (\dict ->
       if Dict.isEmpty dict
       then Json.Decode.succeed val
       else Json.Decode.fail "expected empty JSON object")


(:=) : String -> Decoder a -> Decoder a
(:=) path decoder =
  at [ path ] decoder

url : String -> List (String, String) -> String
url endpoint params =
  endpoint ++ "?" ++ String.join "&" (List.map (\(k, v) -> k ++ "=" ++ v) params)
