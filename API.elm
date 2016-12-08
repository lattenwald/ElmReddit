module API
  exposing (..)

import Navigation
import Dict
import String
import Http
import Html exposing (Html, div, a, text, button)
import Html.Attributes exposing (href)
import Base64
import Result
import Json.Decode as Json exposing (Decoder, succeed, string, int)
import Utils exposing (..)
import Config


-- TODO store expires_in and modify it (subscription to time might help)
-- TODO after that it might be of some use to make token permanent, refresh it
-- TODO and may be store it in browser storage? or hash even? maybe better only refresh token?
-- TYPES


type alias Token =
  { access_token : String
  , token_type : String
  , scope : List String
  }


decodeToken : Decoder Token
decodeToken =
  Json.map3
    Token
    ("access_token" := string)
    ("token_type" := string)
    ("scope" := Json.map (String.split " ") string)


type alias Identity =
  { name : String
  , link_karma : Int
  , comment_karma : Int
  }


decodeIdentity : Decoder Identity
decodeIdentity =
  Json.map3
    Identity
    ("name" := string)
    ("link_karma" := int)
    ("comment_karma" := int)


type alias Code =
  String


type alias Model =
  { token : Maybe Token
  , error : Maybe Http.Error
  , identity : Maybe Identity
  }


type Msg
  = Noop
  | GotCode Code
  | GotError Http.Error
  | GotToken Token
  | GetIdentity
  | GotIdentity Identity
  | Loaded



-- MODEL VIEW UPDATE


model : Model
model =
  { token = Nothing, error = Nothing, identity = Nothing }


init : Navigation.Location -> ( Model, Cmd Msg )
init loc = model ! [fire <| urlParser loc]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GetIdentity ->
      model ! [ getIdentity model.token ]

    GotError error ->
      { model | error = Just error } ! []

    GotCode code ->
      { model
        | error = Nothing
      }
        ! [ getToken code ]

    GotToken token ->
      { model
        | token = Just token
        , error = Nothing
      } ! [ getIdentity (Just token)
          , Navigation.newUrl "?"
          ]

    GotIdentity idty ->
      ignore (Debug.log "IDTY" idty) <|
        { model
          | identity = Just idty
        } ! [ fire Loaded ]

    Noop ->
      model ! []

    Loaded ->
      model ! []


urlUpdate : Maybe String -> Model -> ( Model, Cmd Msg )
urlUpdate _ model =
  model ! []


view : Model -> Html Msg
view { token, error, identity } =
  div []
    [ case token of
        Nothing ->
          div [] [ a [ href authorizeUrl ] [ text "authorize" ] ]

        Just t ->
          div [] [ text t.access_token ]
    , case error of
        Nothing ->
          div [] []

        Just err ->
          div [] [ text (toString err) ]
    , case identity of
        Nothing ->
          div [] []

        Just idty ->
          div []
            [ text
                (idty.name
                  ++ " "
                  ++ toString idty.link_karma
                  ++ "·"
                  ++ toString idty.comment_karma
                )
            ]
    ]



-- EXTERNAL API

urlParser : Navigation.Location -> Msg
urlParser loc = loc.search
              |> getCodeFromSearch
              |> Maybe.map GotCode
              |> Maybe.withDefault Noop


delete : Maybe Token -> (Http.Error -> msg) -> msg -> String
       -> Cmd msg
delete maybeToken fail success url =
  let
    request = req "DELETE" url maybeToken Http.emptyBody Http.expectString
    responseHandler resp =
      case resp of
        Err err -> fail err
        Ok _    -> success
  in
    Http.send responseHandler request


post : Maybe Token -> (Http.Error -> msg) -> (data -> msg)
     -> String -> Http.Body -> Decoder data
     -> Cmd msg
post maybeToken fail success url body decoder =
  let
    request = req "POST" url maybeToken body (Http.expectJson decoder)
    responseHandler resp =
      case resp of
        Err err -> fail err
        Ok data -> success data
  in
    Http.send responseHandler request


put : Maybe Token -> (Http.Error -> msg) -> (data -> msg)
    -> String -> Http.Body -> Decoder data
    -> Cmd msg
put maybeToken fail success url body decoder =
  let
    request = req "PUT" url maybeToken body (Http.expectJson decoder)
    responseHandler resp =
      case resp of
        Err err -> fail err
        Ok data -> success data
  in
    Http.send responseHandler request


req : String -> String -> Maybe Token -> Http.Body -> Http.Expect a
    -> Http.Request a
req method url maybeToken body expect =
  Http.request
        { method = method
        , headers = [Http.header "Authorization" ("bearer " ++ (maybeToken |> Maybe.map .access_token |> Maybe.withDefault ""))]
        , url = url
        , body = body
        , timeout = Nothing
        , expect = expect
        , withCredentials = False
        }



get : Maybe Token -> (Http.Error -> msg) -> (data -> msg)
    -> String -> Decoder data
    -> Cmd msg
get maybeToken fail handler url decoder =
  let
    request = req "GET" url maybeToken Http.emptyBody (Http.expectJson decoder)
    responseHandler resp =
      case resp of
        Err err -> fail err
        Ok data -> handler data
  in
    Http.send responseHandler request


-- HELPER FUNCTIONS


authorizeUrl : String
authorizeUrl =
  let
    endpoint =
      "https://www.reddit.com/api/v1/authorize"

    scopes =
      [ "identity", "mysubreddits", "subscribe", "read" ]

    params =
      [ ( "client_id", Config.appId )
      , ( "response_type", "code" )
      , ( "redirect_uri", Config.redirectUrl )
      , ( "duration", "temporary" )
      , ( "scope", String.join "," scopes )
      , ( "state", "none" )
      ]
  in
    url endpoint params


getCodeFromSearch : String -> Maybe Code
getCodeFromSearch s =
  let
    params =
      parseUrlParams s
  in
    Dict.get "code" params


parseUrlParams : String -> Dict.Dict String String
parseUrlParams s =
  let
    folder s acc =
      case String.split "=" s of
        [ k, v ] ->
          Dict.insert k v acc

        _ ->
          acc
  in
    s
      |> String.dropLeft 1
      |> String.split "&"
      |> List.foldl folder Dict.empty


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

getIdentity : Maybe Token -> Cmd Msg
getIdentity token =
  get token GotError GotIdentity "https://oauth.reddit.com/api/v1/me" decodeIdentity

getToken : String -> Cmd Msg
getToken code =
  let
    url =
      "https://www.reddit.com/api/v1/access_token"

    authHeader =
      Http.header "Authorization" ("Basic " ++ Result.withDefault "" (Base64.encode (Config.appId ++ ":")))

    request = Http.request
              { method          = "POST"
              , url             = url
              , headers         = [authHeader]
              , expect          = Http.expectJson decodeToken
              , timeout         = Nothing
              , withCredentials = False
              , body            = Http.multipartBody
                                  [ Http.stringPart "grant_type" "authorization_code"
                                  , Http.stringPart "code" code
                                  , Http.stringPart "redirect_uri" Config.redirectUrl ]
              }

    handler resp =
      case resp of
        Err err  -> GotError err
        Ok token -> GotToken token
  in
    Http.send handler request


main =
  Navigation.program urlParser
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
