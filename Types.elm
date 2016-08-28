module Types exposing (..)

import Dict exposing (Dict)
import Json.Decode as Json
  exposing ( Decoder, object5, string, bool, succeed, (:=), at
           , oneOf, null, object2, list, object3)
import Set exposing (Set)

type alias SubredditName = String
type alias MultiredditName = String
type alias After = String

type alias Subreddit =
  { name         : SubredditName
  , display_name : String
  , subscribed   : Bool
  , link         : String
  , multireddits : Set MultiredditName }

decodeSubreddit : Decoder Subreddit
decodeSubreddit =
  let
    decoder = object5 Subreddit
              ("name"               := string)
              ("display_name"       := string)
              ("user_is_subscriber" := bool)
              ("url"                := string)
              (succeed Set.empty)
  in
    at ["data"] decoder

type alias Subreddits = Dict SubredditName Subreddit

decodeSubreddits : Decoder (Maybe After, Subreddits)
decodeSubreddits =
  let
    nullOr : Decoder a -> Decoder (Maybe a)
    nullOr decoder =
      oneOf [ null Nothing, Json.map Just decoder ]
    decoder = object2 (,)
             ("after" := nullOr string)
             (at ["children"] <|
                Json.map (List.foldl
                            (\item acc ->
                               Dict.insert item.name item acc)
                            Dict.empty) <|
                list decodeSubreddit)
  in
    at ["data"] decoder

type alias Multireddit =
  { name       : MultiredditName
  , link       : String
  , subreddits : Set SubredditName }

decodeMultireddit : Decoder (Multireddit, Subreddits)
decodeMultireddit =
  let
    nameDecoder = ("name" := string)
    listToDict : List Subreddit -> Subreddits
    listToDict =
      let
        folder item acc = Dict.insert item.name item acc
      in
        List.foldl folder Dict.empty
    decoder' = object3 (,,)
              ("name" := string)
              ("path" := string)
              ("subreddits" := list decodeSubreddit)
    mapper (name, path, subs) =
      let
        subNames = Set.fromList <| List.map .name subs
      in
        (Multireddit name path subNames, listToDict subs)
    decoder = Json.map mapper decoder'
  in
    at ["data"] decoder

type alias Multireddits = Dict MultiredditName Multireddit

decodeMultireddits : Decoder (Multireddits, Subreddits)
decodeMultireddits =
  let
    folder (multi, subs) (accMultis, accSubs) =
      ( Dict.insert multi.name multi accMultis
      , Dict.union subs accSubs )
  in
    Json.map
          (List.foldl folder (Dict.empty, Dict.empty))
          (list decodeMultireddit)
