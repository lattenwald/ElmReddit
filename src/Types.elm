module Types exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, at, int, string)
import Json.Encode as JE


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
    JE.object [ ("access_token", JE.string token.access_token)
              , ("token_type", JE.string token.token_type)
              , ("scope", JE.string <| String.join " " token.scope) ]


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


type alias Subreddit =
    { name : SubredditName
    , display_name : String
    , subscribed : Bool
    , link : String
    }


type alias Subreddits =
    Dict SubredditName Subreddit
