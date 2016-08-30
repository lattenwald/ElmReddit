module API exposing ( Token, Model, Msg(..), Code
                    , model, init, update, view, urlUpdate, urlParser
                    , get, post, put, delete
                    , signedSend )

import Navigation
import Dict
import String
import Http
import Html exposing (Html, div, a, text, button)
import Html.Attributes exposing (href)
import Task exposing (Task)
import Base64
import Result
import Json.Decode as Json exposing (Decoder, succeed, string, int, object3, (:=))

import Utils exposing (ignore, fire)
import Config

-- TODO store expires_in and modify it (subscription to time might help)
-- TODO after that it might be of some use to make token permanent, refresh it
-- TODO and may be store it in browser storage? or hash even? maybe better only refresh token?

-- TYPES

type alias Token =
  { access_token : String
  , token_type   : String
  , scope        : List String }

decodeToken : Decoder Token
decodeToken =
  object3
    Token
    ("access_token" := string)
    ("token_type"   := string)
    ("scope"        := Json.map (String.split " ") string)

type alias Identity =
  { name          : String
  , link_karma    : Int
  , comment_karma : Int }

decodeIdentity : Decoder Identity
decodeIdentity =
  object3
    Identity
    ("name"          := string)
    ("link_karma"    := int)
    ("comment_karma" := int)

type alias Code = String

type alias Model =
  { token    : Maybe Token
  , error    : Maybe Http.Error
  , identity : Maybe Identity }

type Msg = GotCode Code
         | GotError Http.Error
         | GotToken Token
         | GetIdentity
         | GotIdentity Identity
         | Noop
         | Loaded

-- MODEL VIEW UPDATE

model : Model
model = {token = Nothing, error = Nothing, identity = Nothing}

init : Maybe Code -> (Model, Cmd Msg)
init code =
  case code of
    Nothing   -> model ! []
    Just code -> model ! [getToken code]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetIdentity -> model ! [getIdentity model.token]

    GotError error -> {model | error = Just error} ! []

    GotCode code ->
      { model | error = Nothing
      } ! [ getToken code ]

    GotToken token -> { model | token = Just token, error = Nothing
                      } ! [ getIdentity (Just token)
                          , Navigation.newUrl "?"]

    GotIdentity idty -> ignore (Debug.log "IDTY" idty) <|
                        { model | identity = Just idty
                        } ! [ fire Loaded ]

    Noop -> model ! []

    Loaded -> model ! []

urlUpdate : Maybe String -> Model -> (Model, Cmd Msg)
urlUpdate _ model = model ! []

view : Model -> Html Msg
view {token, error, identity} =
  div [] [ case token of
             Nothing -> div [] [a [href authorizeUrl] [text "authorize"]]
             Just t  -> div [] [text t.access_token]
         , case error of
             Nothing  -> div [] []
             Just err -> div [] [text (toString err)]
         , case identity of
             Nothing   -> div [] []
             Just idty -> div [] [text (idty.name
                                          ++ " " ++ toString idty.link_karma
                                          ++ "·" ++ toString idty.comment_karma)]
         ]

-- EXTERNAL API

urlParser : Navigation.Parser (Maybe Code)
urlParser =
  Navigation.makeParser (.search >> getCodeFromSearch)

get : Maybe Token -> (Http.Error -> msg) -> (data -> msg) -> String -> Decoder data
    -> Cmd msg
get maybeToken fail success url decoder =
  Maybe.withDefault Cmd.none <|
  flip Maybe.map maybeToken <| \token ->
    Task.perform fail success <|
        Http.fromJson decoder <| signedSend token.access_token "GET" url [] Http.empty

post : Maybe Token -> (Http.Error -> msg) -> (data -> msg) -> String
    -> Http.Body -> Decoder data
    -> Cmd msg
post maybeToken fail success url body decoder =
  Maybe.withDefault Cmd.none <|
  flip Maybe.map maybeToken <| \token ->
    Task.perform fail success <|
        Http.fromJson decoder <| signedSend token.access_token "POST" url [] body

put : Maybe Token -> (Http.Error -> msg) -> (data -> msg) -> String
    -> Http.Body -> Decoder data
    -> Cmd msg
put maybeToken fail success url body decoder =
  Maybe.withDefault Cmd.none <|
  flip Maybe.map maybeToken <| \token ->
    Task.perform fail success <|
        Http.fromJson decoder <| signedSend token.access_token "PUT" url [] body

delete : Maybe Token -> (Http.Error -> msg) -> (() -> msg) -> String
       -> Cmd msg
delete maybeToken fail success url =
  Maybe.withDefault Cmd.none <|
  flip Maybe.map maybeToken <| \token ->
    let
      handle : Http.Response -> Task Http.Error ()
      handle response =
        if 200 <= response.status && response.status < 300 then
          case response.value of
            Http.Text "" -> Task.succeed ()
            _ ->
              Task.fail (Http.UnexpectedPayload "Response body is a blob, expecting a string.")
        else
          Task.fail (Http.BadResponse response.status response.statusText)
      promoteError : Http.RawError -> Http.Error
      promoteError rawError =
        case rawError of
          Http.RawTimeout -> Http.Timeout
          Http.RawNetworkError -> Http.NetworkError
    in
      Task.perform fail success <|
      Task.mapError promoteError (signedSend token.access_token "DELETE" url [] Http.empty)
            `Task.andThen` handle

-- HELPER FUNCTIONS

authorizeUrl : String
authorizeUrl =
  let
    endpoint    = "https://www.reddit.com/api/v1/authorize"
    scopes      = [ "identity", "mysubreddits", "subscribe", "read" ]
  in
    Http.url endpoint [ ("client_id", Config.appId)
                      , ("response_type", "code")
                      , ("redirect_uri", Config.redirectUrl)
                      , ("duration", "temporary")
                      , ("scope", String.join "," scopes)
                      , ("state", "none") ]

getCodeFromSearch : String -> Maybe Code
getCodeFromSearch s =
  let
    params = parseUrlParams s
  in
    Dict.get "code" params

parseUrlParams : String -> Dict.Dict String String
parseUrlParams s =
  let folder s acc =
        case String.split "=" s of
          [k, v] -> Dict.insert k v acc
          _      -> acc
  in
    s |> String.dropLeft 1
      |> String.split "&"
      |> List.foldl folder Dict.empty

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

signedSend :  String -> String -> String -> List (String, String) -> Http.Body
           -> Task Http.RawError Http.Response
signedSend token verb url headers body =
  let
    authHeader = ("Authorization", "bearer " ++ token)
    request    =
      { verb    = verb
      , headers = [authHeader, ("Accept", "application/json")] ++ headers
      , url     = url
      , body    = body }
  in
    Http.send Http.defaultSettings request

getIdentity : Maybe Token -> Cmd Msg
getIdentity token =
  get token GotError GotIdentity "https://oauth.reddit.com/api/v1/me" decodeIdentity

getToken : String -> Cmd Msg
getToken code =
  let
    url = "https://www.reddit.com/api/v1/access_token"
    authHeader = ( "Authorization"
                 , "Basic " ++ Result.withDefault "" (Base64.encode (Config.appId ++ ":")) )
    request =
      { verb = "POST"
      , url = url
      , headers = [authHeader, ("Content-Type", "application/x-www-form-urlencoded")]
      , body = Http.string <| "grant_type=authorization_code&code=" ++ code
               ++ "&redirect_uri=" ++ Config.redirectUrl
      }
  in
    Task.perform GotError GotToken <|
        Http.fromJson decodeToken <|
        Http.send Http.defaultSettings request

main =
  Navigation.program urlParser
    { init = init
    , view = view
    , update = update
    , urlUpdate = urlUpdate
    , subscriptions = subscriptions
    }
