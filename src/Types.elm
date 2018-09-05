module Types exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, at, int, string)
import Json.Encode as JE
import Set exposing (Set)


type alias After =
    String


type alias Token =
    { access_token : String
    , token_type : String
    , scope : List String
    }


decodeToken : Decoder Token
decodeToken =
    JD.map3
        Token
        (at [ "access_token" ] string)
        (at [ "token_type" ] string)
        (at [ "scope" ] <| JD.map (String.split " ") string)


encodeToken : Token -> JE.Value
encodeToken token =
    JE.object
        [ ( "access_token", JE.string token.access_token )
        , ( "token_type", JE.string token.token_type )
        , ( "scope", JE.string <| String.join " " token.scope )
        ]


type alias Identity =
    { name : String
    , link_karma : Int
    , comment_karma : Int
    }


decodeIdentity : Decoder Identity
decodeIdentity =
    JD.map3
        Identity
        (at [ "name" ] string)
        (at [ "link_karma" ] int)
        (at [ "comment_karma" ] int)


type alias SubredditName =
    String


type alias MultiredditName =
    String


type alias Subreddit =
    { name : SubredditName
    , display_name : String
    , subscribed : Bool
    , link : String
    , multireddits : Set MultiredditName
    }


decodeSubreddit : Decoder Subreddit
decodeSubreddit =
    let
        decoder =
            JD.map5 Subreddit
                (at [ "name" ] string)
                (at [ "display_name" ] string)
                (at [ "user_is_subscriber" ] boolish)
                (at [ "url" ] string)
                (JD.succeed Set.empty)
    in
    at [ "data" ] decoder


type alias Subreddits =
    Dict SubredditName Subreddit


decodeSubreddits : Decoder ( Maybe After, Subreddits )
decodeSubreddits =
    let
        tup2 a b =
            ( a, b )

        decoder =
            JD.map2 tup2
                (at [ "after" ] <| nullOr string)
                (at [ "children" ] <|
                    JD.map
                        (List.foldl
                            (\item acc ->
                                Dict.insert item.name item acc
                            )
                            Dict.empty
                        )
                    <|
                        JD.list decodeSubreddit
                )
    in
    at [ "data" ] decoder


type alias Multireddit =
    { name : MultiredditName
    , link : String
    , subreddits : Set SubredditName
    }


decodeMultireddit : Decoder ( Multireddit, Subreddits )
decodeMultireddit =
    let
        nameDecoder =
            at [ "name" ] string

        listToDict : List Subreddit -> Subreddits
        listToDict =
            let
                folder item acc =
                    Dict.insert item.name item acc
            in
            List.foldl folder Dict.empty

        tup3 a b c =
            ( a, b, c )

        decoder_ =
            JD.map3 tup3
                (at [ "name" ] string)
                (at [ "path" ] string)
                (at [ "subreddits" ] <| JD.list decodeSubreddit)

        mapper ( name, path, subs ) =
            let
                subNames =
                    Set.fromList <| List.map .name subs
            in
            ( Multireddit name path subNames, listToDict subs )

        decoder =
            JD.map mapper decoder_
    in
    at [ "data" ] decoder


type alias Multireddits =
    Dict MultiredditName Multireddit


decodeMultireddits : Decoder ( Multireddits, Subreddits )
decodeMultireddits =
    let
        folder ( multi, subs ) ( accMultis, accSubs ) =
            ( Dict.insert multi.name multi accMultis
            , Dict.union subs accSubs
            )
    in
    JD.map
        (List.foldl folder ( Dict.empty, Dict.empty ))
        (JD.list decodeMultireddit)


nullOr : Decoder a -> Decoder (Maybe a)
nullOr decoder =
    JD.oneOf [ JD.null Nothing, JD.map Just decoder ]


boolish : Decoder Bool
boolish =
    JD.map (Maybe.withDefault False) (JD.maybe JD.bool)
