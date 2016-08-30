module Utils exposing (..)

import Task
import Json.Decode
import Dict
import Result

unique : List comparable -> List comparable
unique l =
  let
    folder item acc =
      case acc of
        [] -> [item]
        head::rest -> if item == head
                      then acc
                      else item::acc
  in
    l |> List.sort
      |> List.foldl folder []
      |> List.reverse

diff : List comparable -> List comparable -> List comparable
diff a b =
  let
    a_sorted = List.sort a
    b_sorted = List.sort b
    diff' a b acc =
      case (a, b) of
        ([], _) -> acc
        (_, []) -> a ++ acc
        (a'::arest, b'::brest) ->
          case a' `compare` b' of
            EQ -> diff' arest brest acc
            LT -> diff' arest b (a'::acc)
            GT -> diff' a brest acc
  in
    diff' a_sorted b_sorted []

ignore : ignored -> a -> a
ignore _ a = a

maybeToList : Maybe a -> List a
maybeToList a =
  case a of
    Nothing -> []
    Just a  -> [a]

fire : a -> Cmd a
fire msg = Task.perform identity identity (Task.succeed msg)

fromJsonUnit : a -> Json.Decode.Decoder a
fromJsonUnit val =
  Json.Decode.customDecoder
        (Json.Decode.dict Json.Decode.string) <| \dict ->
          if Dict.isEmpty dict
          then Result.Ok val
          else Result.Err "expected empty JSON object"
