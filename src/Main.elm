port module Main exposing (..)

import Base64
import Browser
import Browser.Navigation as Nav
import Config
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import LocalStorage exposing (LocalStorage)
import LocalStorage.SharedTypes as LS
import Task
import Types exposing (..)
import Url


type alias Model =
    { token : Maybe Token
    , identity : Maybe Identity
    , subreddits : Subreddits
    , key : Nav.Key
    , storage : LocalStorage Msg
    }


type Msg
    = Noop
    | GotError Http.Error
    | GotCode String
    | LoadToken
    | GotToken Token
    | GetIdentity
    | GotIdentity Identity
    | GetSubreddits (Maybe After)
    | GotSubreddits ( Maybe After, Subreddits )
    | ClickedLink Browser.UrlRequest
    | UpdatePorts LS.Operation (Maybe (LS.Ports Msg)) LS.Key LS.Value


ls_prefix =
    "mfreddit"


initPorts : LS.Ports Msg
initPorts =
    LocalStorage.makeRealPorts getItem setItem clear listKeys


initModel key =
    { token = Nothing
    , identity = Nothing
    , subreddits = Dict.empty
    , key = key
    , storage = LocalStorage.make initPorts ls_prefix
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        model =
            initModel
    in
    ( initModel key, fire <| urlParser url )


tokenKey =
    "token"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        GotError err ->
            ignore (Debug.log "GotError" err) <|
                ( model, Cmd.none )

        GotCode code ->
            ( model, getToken code )

        GotToken token ->
            ( { model | token = Just token }
            , Cmd.batch
                [ getIdentity (Just token)
                , Nav.replaceUrl model.key "?"
                , LocalStorage.setItem model.storage tokenKey (encodeToken token)
                ]
            )

        GotIdentity identity ->
            ignore (Debug.log "IDTY" identity) <|
                ( { model | identity = Just identity }, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External url ->
                    ( model, Nav.load url )

                Browser.Internal url ->
                    ( model, Cmd.none )

        LoadToken ->
            case model.token of
                Nothing ->
                    ( model, LocalStorage.getItem model.storage tokenKey )

                _ ->
                    ( model, Cmd.none )

        UpdatePorts operation ports key value ->
            case operation of
                LS.GetItemOperation ->
                    let
                        newToken =
                            JD.decodeValue decodeToken value |> Result.toMaybe
                    in
                    case newToken of
                        Nothing ->
                            ( model, Cmd.none )

                        Just t ->
                            ( { model | token = newToken }, fire <| GotToken t )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> Noop
        , onUrlChange = urlParser
        }


port getItem : LS.GetItemPort msg


port setItem : LS.SetItemPort msg


port clear : LS.ClearPort msg


port listKeys : LS.ListKeysPort msg


port receiveItem : LS.ReceiveItemPort msg


ignore : ignored -> a -> a
ignore _ a =
    a


getIdentity : Maybe Token -> Cmd Msg
getIdentity token =
    get token GotError GotIdentity "https://oauth.reddit.com/api/v1/me" decodeIdentity


getToken : String -> Cmd Msg
getToken code =
    let
        url =
            "https://www.reddit.com/api/v1/access_token"

        authHeader =
            Http.header "Authorization" ("Basic " ++ Base64.encode (Config.appId ++ ":"))

        request =
            Http.request
                { method = "POST"
                , url = url
                , headers = [ authHeader ]
                , expect = Http.expectJson decodeToken
                , timeout = Nothing
                , withCredentials = False
                , body =
                    Http.multipartBody
                        [ Http.stringPart "grant_type" "authorization_code"
                        , Http.stringPart "code" code
                        , Http.stringPart "redirect_uri" Config.redirectUrl
                        ]
                }

        handler resp =
            case resp of
                Err err ->
                    GotError err

                Ok token ->
                    GotToken token
    in
    Http.send handler request


view : Model -> Browser.Document Msg
view model =
    let
        viewIdentity =
            nav [ classList [ ( "navbar", True ), ( "navbar-expand-md", True ), ( "navbar-light", True ), ( "bg-light", True ) ] ]
                [ case model.identity of
                    Nothing ->
                      a [ classList [ ( "btn", True ), ( "btn-primary", True ) ]
                        , href authorizeUrl
                        , onClick (ClickedLink (Browser.External authorizeUrl))
                        ]
                      [ text "authorize" ]

                    Just idty ->
                        a [ class "navbar-brand", href "#" ] [ text idty.name
                                                             , text " ("
                                                             , text <| String.fromInt idty.link_karma
                                                             , text " · "
                                                             , text <| String.fromInt idty.comment_karma
                                                             , text ")"]
                ]

    in
    { title = "MFReddit"
    , body =
        [ div [ class "fluid-container" ] [ viewIdentity ] ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        prefix_ =
            LocalStorage.getPrefix model.storage
    in
    receiveItem <| LS.receiveWrapper UpdatePorts prefix_


urlParser : Url.Url -> Msg
urlParser loc =
    loc.query
        |> Maybe.andThen getCodeFromSearch
        |> Maybe.map GotCode
        |> Maybe.withDefault LoadToken


get :
    Maybe Token
    -> (Http.Error -> msg)
    -> (data -> msg)
    -> String
    -> Decoder data
    -> Cmd msg
get maybeToken fail handler url decoder =
    let
        request =
            req "GET" url maybeToken Http.emptyBody (Http.expectJson decoder)

        responseHandler resp =
            case resp of
                Err err ->
                    fail err

                Ok data ->
                    handler data
    in
    Http.send responseHandler request


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
    makeUrl endpoint params


getCodeFromSearch : String -> Maybe String
getCodeFromSearch s =
    let
        params =
            parseUrlParams s
    in
    Dict.get "code" params


req :
    String
    -> String
    -> Maybe Token
    -> Http.Body
    -> Http.Expect a
    -> Http.Request a
req method url maybeToken body expect =
    Http.request
        { method = method
        , headers = [ Http.header "Authorization" ("bearer " ++ (maybeToken |> Maybe.map .access_token |> Maybe.withDefault "")) ]
        , url = url
        , body = body
        , timeout = Nothing
        , expect = expect
        , withCredentials = False
        }


makeUrl : String -> List ( String, String ) -> String
makeUrl endpoint params =
    endpoint ++ "?" ++ String.join "&" (List.map (\( k, v ) -> k ++ "=" ++ v) params)


parseUrlParams : String -> Dict.Dict String String
parseUrlParams s =
    let
        folder s_ acc =
            case String.split "=" s_ of
                [ k, v ] ->
                    Dict.insert k v acc

                _ ->
                    acc
    in
    s
        |> String.dropLeft 1
        |> String.split "&"
        |> List.foldl folder Dict.empty


fire : a -> Cmd a
fire msg =
    Task.perform identity (Task.succeed msg)
