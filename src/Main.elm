port module Main exposing (..)

import Base64
import Browser
import Browser.Navigation as Nav
import Config
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, id, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import LocalStorage exposing (LocalStorage)
import LocalStorage.SharedTypes as LS
import Set exposing (Set)
import Types exposing (..)
import Url
import Utils exposing (..)


type Focused
    = FSub (Maybe SubredditName)
    | FNotInMulti
    | FNotSubscribed
    | FMulti (Maybe MultiredditName)


type alias Model =
    { token : Maybe Token
    , identity : Maybe Identity
    , subreddits : Subreddits
    , multireddits : Multireddits
    , focus : Focused
    , choosingMultiForSub : Maybe Subreddit
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
    | GotMultireddits ( Multireddits, Subreddits )
    | SetFocus Focused
    | ScrollTo Focused
    | ChooseMultiForSub Subreddit
    | AddSubToMulti Subreddit Multireddit
    | AddedSubToMulti Multireddit Subreddit
    | RemoveSubFromMulti Subreddit Multireddit
    | RemovedSubFromMulti Multireddit Subreddit
    | Subscribe Subreddit
    | Subscribed Subreddit
    | Unsubscribe Subreddit
    | Unsubscribed Subreddit
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
    , multireddits = Dict.empty
    , focus = FSub Nothing
    , choosingMultiForSub = Nothing
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
                case err of
                    Http.BadStatus resp ->
                        case resp.status.code of
                            401 ->
                                ( { model | token = Nothing }
                                , LocalStorage.clear model.storage
                                )

                            _ ->
                                ( model, Cmd.none )

                    _ ->
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
                ( { model | identity = Just identity }, fire <| GetSubreddits Nothing )

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

        GetSubreddits after ->
            ( model, getSubreddits model.token after )

        GotSubreddits ( after, subreddits ) ->
            let
                nextCommand =
                    case after of
                        Nothing ->
                            getMultireddits model.token

                        Just _ ->
                            getSubreddits model.token after
            in
            ( { model | subreddits = Dict.union subreddits model.subreddits }
            , nextCommand
            )

        GotMultireddits ( multireddits, subreddits ) ->
            let
                subMultis : Dict SubredditName (Set MultiredditName)
                subMultis =
                    let
                        folder :
                            MultiredditName
                            -> Multireddit
                            -> Dict SubredditName (Set MultiredditName)
                            -> Dict SubredditName (Set MultiredditName)
                        folder mname multireddit acc =
                            let
                                folder_ :
                                    SubredditName
                                    -> Dict SubredditName (Set MultiredditName)
                                    -> Dict SubredditName (Set MultiredditName)
                                folder_ sname acc_ =
                                    Dict.insert sname
                                        (Maybe.withDefault (Set.singleton mname) <|
                                            Maybe.map (\set -> Set.insert mname set) <|
                                                Dict.get sname acc
                                        )
                                        acc_
                            in
                            Set.foldl folder_ acc multireddit.subreddits
                    in
                    Dict.foldl folder Dict.empty multireddits

                newSubreddits : Subreddits
                newSubreddits =
                    let
                        subWithMultis =
                            Dict.map
                                (\sname s ->
                                    { s | multireddits = Maybe.withDefault Set.empty (Dict.get sname subMultis) }
                                )
                    in
                    subWithMultis <| Dict.union subreddits model.subreddits
            in
            ( { model | subreddits = newSubreddits, multireddits = multireddits }, Cmd.none )

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

        SetFocus newFocus ->
            ( { model | focus = newFocus, choosingMultiForSub = Nothing }, Cmd.none )

        ScrollTo newFocus ->
            let
                scrollCommand =
                    case newFocus of
                        FSub maybeSubredditName ->
                            maybeSubredditName
                                |> Maybe.andThen (\s -> Dict.get s model.subreddits)
                                |> Maybe.map (\s -> scrollTo (subredditId s))
                                |> Maybe.withDefault Cmd.none

                        FMulti maybeMultiredditName ->
                            maybeMultiredditName
                                |> Maybe.andThen (\m -> Dict.get m model.multireddits)
                                |> Maybe.map (\m -> scrollTo (multiredditId m))
                                |> Maybe.withDefault Cmd.none

                        FNotInMulti ->
                            scrollTo <| multiredditId <| mNotInMulti []

                        FNotSubscribed ->
                            scrollTo <| multiredditId <| mNotSubscribed []
            in
            ( { model | focus = newFocus }, scrollCommand )

        GetIdentity ->
            ( model, getIdentity model.token )

        ChooseMultiForSub s ->
            ( { model | choosingMultiForSub = Just s, focus = FMulti Nothing }, Cmd.none )

        AddSubToMulti subreddit multireddit ->
            ( { model | choosingMultiForSub = Nothing, focus = FSub (Just subreddit.name) }
            , addToMulti model.token subreddit multireddit
            )

        AddedSubToMulti m s ->
            let
                oldSub =
                    Dict.get s.name model.subreddits |> Maybe.withDefault s

                newSub =
                    { oldSub | multireddits = Set.insert m.name oldSub.multireddits }

                newSubs =
                    Dict.insert newSub.name newSub model.subreddits

                oldMulti =
                    Dict.get m.name model.multireddits |> Maybe.withDefault m

                newMulti =
                    { oldMulti | subreddits = Set.insert s.name oldMulti.subreddits }

                newMultis =
                    Dict.insert newMulti.name newMulti model.multireddits
            in
            ( { model | subreddits = newSubs, multireddits = newMultis }
            , Cmd.none
            )

        RemoveSubFromMulti subreddit multireddit ->
            ( model, removeSubFromMulti model.token subreddit multireddit )

        RemovedSubFromMulti multireddit subreddit ->
            let
                subUpdater s =
                    { s | multireddits = Set.remove multireddit.name s.multireddits }

                newSubs =
                    Dict.update subreddit.name (Maybe.map subUpdater) model.subreddits

                multiUpdater m =
                    { m | subreddits = Set.remove subreddit.name m.subreddits }

                newMultis =
                    Dict.update multireddit.name (Maybe.map multiUpdater) model.multireddits
            in
            ( { model | subreddits = newSubs, multireddits = newMultis }, Cmd.none )

        Unsubscribe subreddit ->
            ( model, unsubscribe model.token subreddit )

        Unsubscribed subreddit ->
            ( { model | subreddits = Dict.insert subreddit.name subreddit model.subreddits }
            , Cmd.none
            )

        Subscribe subreddit ->
            ( model, subscribe model.token subreddit )

        Subscribed subreddit ->
            ( { model | subreddits = Dict.insert subreddit.name subreddit model.subreddits }
            , Cmd.none
            )


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
        viewNavbar =
            let
                viewAuthButton =
                    a
                        [ classList [ ( "btn", True ), ( "btn-primary", True ) ]
                        , href authorizeUrl
                        , onClick (ClickedLink (Browser.External authorizeUrl))
                        ]
                        [ text "authorize" ]

                viewIdentity idty =
                    a [ class "navbar-brand", href "#" ]
                        [ text idty.name
                        , text " ("
                        , text <| String.fromInt idty.link_karma
                        , text " · "
                        , text <| String.fromInt idty.comment_karma
                        , text ")"
                        ]

                viewNavbarItems =
                    case model.choosingMultiForSub of
                        Nothing ->
                            let
                                subs_tab =
                                    case model.focus of
                                        FSub _ ->
                                            True

                                        _ ->
                                            False

                                multis_tab =
                                    not subs_tab
                            in
                            ul [ classList [ ( "nav", True ), ( "nav-tabs", True ), ( "flex-row", True ) ] ]
                                [ li [ class "nav-item" ]
                                    [ a
                                        [ classList
                                            [ ( "btn", True )
                                            , ( "btn-primary", subs_tab )
                                            , ( "text-dark", not subs_tab )
                                            ]
                                        , href "#subs"
                                        , onClick (SetFocus <| FSub Nothing)
                                        ]
                                        [ text "Subs" ]
                                    ]
                                , li [ class "nav-item" ]
                                    [ a
                                        [ classList
                                            [ ( "btn", True )
                                            , ( "btn-primary", multis_tab )
                                            , ( "text-dark", not multis_tab )
                                            ]
                                        , href "#multis"
                                        , onClick (SetFocus <| FMulti Nothing)
                                        ]
                                        [ text "Multis" ]
                                    ]
                                ]

                        Just s ->
                            span
                                [ classList
                                    [ ( "text-dark", True )
                                    , ( "navbar-text", True )
                                    , ( "yellow", True )
                                    , ( "lighten-5", True )
                                    , ( "px-5", True )
                                    , ( "pointer", True )
                                    ]
                                , onClick <| SetFocus <| FSub <| Just s.name
                                ]
                                [ text <| "Cancel adding " ++ s.link ++ " to multireddit" ]
            in
            nav
                [ id "navbar"
                , classList
                    [ ( "navbar", True )
                    , ( "navbar-expand-md", True )
                    , ( "navbar-light", True )
                    , ( "bg-light", True )
                    , ( "sticky-top", True )
                    ]
                ]
                (case model.identity of
                    Nothing ->
                        [ viewAuthButton ]

                    Just idty ->
                        viewIdentity idty :: viewNavbarItems :: []
                )

        viewFocusedSubreddit subreddit =
            let
                multireddits =
                    subreddit.multireddits
                        |> Set.toList
                        |> List.filterMap (\mname -> Dict.get mname model.multireddits)

                viewMulti multireddit =
                    [ a
                        [ href (redditLink "/me/m/" ++ multireddit.name)
                        , class "card-link"
                        , onClick (ScrollTo <| FMulti (Just multireddit.name))
                        ]
                        [ text multireddit.name ]
                    , a
                        [ classList [ ( "card-link", True ), ( "ml-1", True ) ]
                        , href "#"
                        , onClick (RemoveSubFromMulti subreddit multireddit)
                        ]
                        [ text "⨯" ]
                    ]
            in
            div
                [ id <| subredditId subreddit, class "card" ]
                [ div
                    [ classList [ ( "card-header", True ), ( "pointer", True ) ]
                    , onClick <| SetFocus (FSub Nothing)
                    ]
                    [ div [ class "float-right" ] [ text (subreddit.multireddits |> Set.size |> String.fromInt) ]
                    , span
                        [ classList
                            [ ( "text-muted", not subreddit.subscribed )
                            , ( "font-italic", not subreddit.subscribed )
                            ]
                        ]
                        [ text subreddit.link ]
                    ]
                , div [ class "card-body" ]
                    (a [ href "#", class "card-link", onClick (ChooseMultiForSub subreddit) ] [ text "✚" ]
                        :: (multireddits
                                |> List.sortBy (.name >> String.toLower)
                                |> List.concatMap viewMulti
                           )
                    )
                ]

        viewNotFocusedSubreddit subreddit =
            div
                [ id <| subredditId subreddit, class "card" ]
                [ div
                    [ classList [ ( "card-header", True ), ( "pointer", True ) ]
                    , onClick (SetFocus <| FSub (Just subreddit.name))
                    ]
                    [ div [ class "float-right" ] [ text (subreddit.multireddits |> Set.size |> String.fromInt) ]
                    , span
                        [ classList
                            [ ( "text-muted", not subreddit.subscribed )
                            , ( "font-italic", not subreddit.subscribed )
                            ]
                        ]
                        [ text subreddit.link ]
                    ]
                ]

        viewSubreddit subreddit =
            let
                isFocused =
                    model.focus == FSub (Just subreddit.name)
            in
            if isFocused then
                viewFocusedSubreddit subreddit

            else
                viewNotFocusedSubreddit subreddit

        viewSubsTab =
            div [ id "subs" ] (model.subreddits |> Dict.values |> List.sortBy (.link >> String.toLower) |> List.map viewSubreddit)

        viewFocusedMultireddit multireddit =
            let
                subOrUnsub subreddit =
                    let
                        ( msg, txt ) =
                            if subreddit.subscribed then
                                ( Unsubscribe subreddit, "⨯" )

                            else
                                ( Subscribe subreddit, "＋" )
                    in
                    a
                        [ classList [ ( "card-link", True ), ( "ml-1", True ), ( "text-dark", subreddit.subscribed ), ( "text-muted", not subreddit.subscribed ) ]
                        , onClick msg
                        , href "#"
                        ]
                        [ text txt ]

                viewSub subreddit =
                    [ a
                        [ href (redditLink "/r/" ++ subreddit.display_name)
                        , classList [ ( "card-link", True ), ( "text-dark", subreddit.subscribed ), ( "text-muted", not subreddit.subscribed ), ( "font-italic", not subreddit.subscribed ) ]
                        , onClick (ScrollTo <| FSub (Just subreddit.name))
                        ]
                        [ text subreddit.display_name ]
                    , subOrUnsub subreddit
                    ]
            in
            div
                [ id <| multiredditId multireddit, class "card" ]
                [ div
                    [ classList [ ( "card-header", True ), ( "pointer", True ) ]
                    , onClick (SetFocus <| FMulti Nothing)
                    ]
                    [ div [ class "float-right" ] [ text (multireddit.subreddits |> Set.size |> String.fromInt) ]
                    , text multireddit.name
                    ]
                , div [ class "card-body" ]
                    (multireddit.subreddits
                        |> Set.toList
                        |> List.filterMap (\sname -> Dict.get sname model.subreddits)
                        |> List.sortBy (.display_name >> String.toLower)
                        |> List.concatMap viewSub
                    )
                ]

        viewNotFocusedMultireddit multireddit =
            div
                [ id <| multiredditId multireddit, class "card" ]
                [ div
                    [ classList [ ( "card-header", True ), ( "pointer", True ) ]
                    , onClick (SetFocus <| FMulti (Just multireddit.name))
                    ]
                    [ div [ class "float-right" ] [ text (multireddit.subreddits |> Set.size |> String.fromInt) ]
                    , text multireddit.name
                    ]
                ]

        viewMultiredditToAdd subreddit multireddit =
            div [ class "card" ]
                [ div
                    [ classList [ ( "card-header", True ), ( "pointer", True ) ]
                    , onClick (AddSubToMulti subreddit multireddit)
                    ]
                    [ div [ class "float-right" ] [ text (multireddit.subreddits |> Set.size |> String.fromInt) ]
                    , text <| "✚ " ++ multireddit.name
                    ]
                ]

        viewMultireddit multireddit =
            let
                isFocused =
                    model.focus == FMulti (Just multireddit.name)
            in
            if isFocused then
                viewFocusedMultireddit multireddit

            else
                viewNotFocusedMultireddit multireddit

        viewMultisTab =
            let
                multiNames =
                    model.multireddits |> Dict.values |> List.sortBy (.name >> String.toLower)

                viewMultisList =
                    case model.choosingMultiForSub of
                        Nothing ->
                            (mNotInMulti (Dict.values model.subreddits) :: mNotSubscribed (Dict.values model.subreddits) :: multiNames) |> List.map viewMultireddit

                        Just s ->
                            multiNames |> List.map (viewMultiredditToAdd s)
            in
            div [ id "multis" ] viewMultisList
    in
    { title = "MFReddit"
    , body =
        [ viewNavbar
        , main_ [ class "fluid-container" ]
            [ case model.focus of
                FSub _ ->
                    viewSubsTab

                FNotInMulti ->
                    viewMultisTab

                FNotSubscribed ->
                    viewMultisTab

                FMulti _ ->
                    viewMultisTab
            ]
        ]
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


getSubreddits : Maybe Token -> Maybe After -> Cmd Msg
getSubreddits token after =
    let
        url =
            makeUrl
                "https://oauth.reddit.com/subreddits/mine/subscriber"
            <|
                maybeToList (Maybe.map (\a -> ( "after", a )) after)
    in
    get token GotError GotSubreddits url decodeSubreddits


getMultireddits : Maybe Token -> Cmd Msg
getMultireddits token =
    let
        url =
            makeUrl
                "https://oauth.reddit.com/api/multi/mine"
                [ ( "expand_srs", "1" ) ]
    in
    get token GotError GotMultireddits url decodeMultireddits


setFocus : Focused -> Cmd Msg
setFocus newFocus =
    Cmd.map (\_ -> SetFocus newFocus) Cmd.none


redditLink : String -> String
redditLink link =
    "https://old.reddit.com" ++ link


subredditId s =
    "s-" ++ unspace s.name


multiredditId m =
    "m-" ++ unspace m.name


port scrollTo : String -> Cmd msg


mNotInMulti subreddits =
    { name = "not in multireddit"
    , subreddits =
        List.filterMap
            (\s ->
                if Set.isEmpty s.multireddits then
                    Just s.name

                else
                    Nothing
            )
            subreddits
            |> Set.fromList
    , link = ""
    }


mNotSubscribed subreddits =
    { name = "not subscribed"
    , subreddits =
        List.filterMap
            (\s ->
                if not s.subscribed then
                    Just s.name

                else
                    Nothing
            )
            subreddits
            |> Set.fromList
    , link = ""
    }


addToMulti : Maybe Token -> Subreddit -> Multireddit -> Cmd Msg
addToMulti token subreddit multireddit =
    let
        url =
            "https://oauth.reddit.com/api/multi" ++ multireddit.link ++ "/r/" ++ subreddit.display_name

        body =
            Http.multipartBody [ Http.stringPart "model" <| JE.encode 0 <| JE.object [ ( "name", JE.string subreddit.display_name ) ] ]

        decoder : JD.Decoder Subreddit
        decoder =
            JD.at [ "name" ] JD.string
                |> JD.andThen
                    (\decodedName ->
                        if decodedName == subreddit.display_name then
                            JD.succeed subreddit

                        else
                            JD.fail <| "expected \"name\" equal to \"" ++ subreddit.display_name ++ "\", hot \"" ++ decodedName ++ "\""
                    )
    in
    put token GotError (AddedSubToMulti multireddit) url body decoder


removeSubFromMulti : Maybe Token -> Subreddit -> Multireddit -> Cmd Msg
removeSubFromMulti token subreddit multireddit =
    let
        url =
            "https://oauth.reddit.com/api/multi" ++ multireddit.link ++ "/r/" ++ subreddit.display_name
    in
    delete token GotError (RemovedSubFromMulti multireddit subreddit) url


subscribe : Maybe Token -> Subreddit -> Cmd Msg
subscribe token subreddit =
    let
        body =
            Http.multipartBody
                [ Http.stringPart "action" "sub"
                , Http.stringPart "sr" subreddit.name
                ]
    in
    post token
        GotError
        Subscribed
        "https://oauth.reddit.com/api/subscribe"
        body
        (fromJsonUnit { subreddit | subscribed = True })


fromJsonUnit : a -> JD.Decoder a
fromJsonUnit val =
    JD.dict JD.string
        |> JD.andThen
            (\dict ->
                if Dict.isEmpty dict then
                    JD.succeed val

                else
                    JD.fail "expected empty JSON object"
            )


unsubscribe : Maybe Token -> Subreddit -> Cmd Msg
unsubscribe token subreddit =
    let
        body =
            Http.multipartBody
                [ Http.stringPart "action" "unsub"
                , Http.stringPart "sr" subreddit.name
                ]
    in
    post token
        GotError
        Unsubscribed
        "https://oauth.reddit.com/api/subscribe"
        body
        (fromJsonUnit { subreddit | subscribed = False })
