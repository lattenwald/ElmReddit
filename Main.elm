module Main exposing (..)

-- TODOOO features:
-- TODO create new multi
-- TODO remove empty multi

import API
import Navigation
import Dict exposing (Dict)
import Html.App as App
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, style, class, classList)
import Http exposing (stringData)
import Json.Encode
import Json.Decode exposing (at)
import String
import Set exposing (Set)

import Utils exposing (..)
import Types exposing (..)

type Focused = FNone
             | FMulti MultiredditName
             | FAll
             | FSubscribed
             | FNotSubscribed
             | FNotInMulti

type State = Focus Focused
           | ChoosingMulti Subreddit Focused -- ChoosingMulti subreddit.display_name Focused

type alias Model =
  { apidata      : API.Model
  , subreddits   : Subreddits
  , multireddits : Multireddits
  , state        : State
  }

emptyModel : Model
emptyModel =
  { apidata      = API.model
  , subreddits   = Dict.empty
  , multireddits = Dict.empty
  , state        = Focus FAll }

type Msg = Noop
         | GotError Http.Error
         | Api API.Msg
         | Load
         | GetSubreddits (Maybe After)
         | GotSubreddits (Maybe After, Subreddits)
         | GotMultireddits (Multireddits, Subreddits)
         | SetFocus Focused
         | ChooseMulti Subreddit
         | AddToMulti Subreddit Multireddit Focused -- subreddit.display_name multireddit.link Focused
         | AddedToMulti Multireddit Subreddit
         | RemoveFromMulti Subreddit Multireddit
         | RemovedFromMulti Multireddit Subreddit
         | Subscribe Subreddit
         | Subscribed Subreddit
         | Unsubscribe Subreddit
         | Unsubscribed Subreddit

main =
  Navigation.program API.urlParser
    { init = init
    , view = view
    , update = update
    , urlUpdate = urlUpdate
    , subscriptions = subscriptions
    }

init : Maybe API.Code -> (Model, Cmd Msg)
init code =
  let
    (apidata, cmd) = API.init code
  in
    { emptyModel | apidata = apidata } ! [Cmd.map Api cmd]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop ->
      model ! []

    GotError err -> -- TODO show this somewhere but console
      ignore (Debug.log "GotError" err) <|
      model ! [Cmd.none]

    Api API.Loaded ->
      model ! [fire Load]

    Load -> {
      model
    | subreddits   = Dict.empty
    , multireddits = Dict.empty
    } ! [fire (GetSubreddits Nothing)]

    Api msg -> let (apidata, apimsg) = API.update msg model.apidata
               in  { model | apidata = apidata
                   } ! [ Cmd.map Api apimsg ]

    GetSubreddits after ->
      model ! [getSubreddits model.apidata.token after]

    GotSubreddits (after, subreddits) ->
      { model | subreddits = Dict.union subreddits model.subreddits
      } ! [ case after of
              Nothing -> getMultireddits model.apidata.token
              Just a  -> getSubreddits model.apidata.token after ]

    GotMultireddits (multireddits, subreddits) ->
      let
        subMultis : Dict SubredditName (Set MultiredditName)
        subMultis =
          let
            maybeCons : comparable -> Maybe (Set comparable) -> Maybe (Set comparable)
            maybeCons item maybeList =
              case maybeList of
                Nothing   -> Just <| Set.singleton item
                Just set  -> Just <| Set.insert item set
            folder : MultiredditName -> Multireddit
                   -> Dict SubredditName (Set MultiredditName)
                   -> Dict SubredditName (Set MultiredditName)
            folder mname {subreddits} acc =
              Set.foldl
                    (\sname acc' ->
                              Dict.update sname (maybeCons mname) acc')
                    acc subreddits
          in
            Dict.foldl folder Dict.empty multireddits
        newSubreddits : Subreddits
        newSubreddits =
          Dict.union subreddits model.subreddits |>
          Dict.map (\sname s ->
                      { s | multireddits = Maybe.withDefault
                                           Set.empty
                                           (Dict.get sname subMultis)})
      in {
        model
      | subreddits = newSubreddits
      , multireddits = multireddits
      } ! [Cmd.none]

    SetFocus focused ->
      { model | state = Focus focused
      } ! [Cmd.none]

    ChooseMulti subreddit ->
      let
        returnFocus = case model.state of
                        Focus f -> f
                        _       -> FAll
      in
        { model
          | state = ChoosingMulti subreddit returnFocus
        } ! [Cmd.none]
    AddToMulti subreddit multireddit returnFocus ->
      { model
        | state = Focus returnFocus
      } ! [addToMulti model.apidata.token subreddit multireddit]
    AddedToMulti m s ->
      let
        subUpdater : Maybe Subreddit -> Maybe Subreddit
        subUpdater maybeSubreddit =
          case maybeSubreddit of
            Nothing -> Just s
            Just subreddit -> Just { subreddit | multireddits = Set.insert m.name subreddit.multireddits}
        newSubreddits = Dict.update s.name subUpdater model.subreddits
        multiUpdater : Maybe Multireddit -> Maybe Multireddit
        multiUpdater maybeMulti =
          case maybeMulti of
            Nothing -> Just m
            Just multi -> Just { multi | subreddits = Set.insert s.name multi.subreddits }
        newMultireddits = Dict.update m.name multiUpdater model.multireddits
      in
        { model
          | subreddits = newSubreddits
          , multireddits = newMultireddits
        } ! [Cmd.none]
    RemoveFromMulti s m ->
      model ! [removeFromMulti model.apidata.token s m]
    RemovedFromMulti m s ->
      let
        subUpdater : Maybe Subreddit -> Maybe Subreddit
        subUpdater maybeSubreddit =
          case maybeSubreddit of
            Nothing -> Just s -- XXX logis is ok?
            Just subreddit -> Just { subreddit | multireddits = Set.remove m.name subreddit.multireddits }
        newSubreddits = Dict.update s.name subUpdater model.subreddits
        multiUpdater : Maybe Multireddit -> Maybe Multireddit
        multiUpdater maybeMulti =
          case maybeMulti of
            Nothing -> Just m -- XXX logic is ok?
            Just multi -> Just { multi | subreddits = Set.remove s.name multi.subreddits }
        newMultireddits = Dict.update m.name multiUpdater model.multireddits
      in
        { model
          | subreddits = newSubreddits
          , multireddits = newMultireddits
        } ! [Cmd.none]
    Subscribe s ->
      model ! [subscribe model.apidata.token s]
    Subscribed s ->
      { model
        | subreddits = Dict.insert s.name s model.subreddits
      } ! [Cmd.none]
    Unsubscribe s ->
      model ! [unsubscribe model.apidata.token s]
    Unsubscribed s ->
      { model
        | subreddits = Dict.insert s.name s model.subreddits
      } ! [Cmd.none]

view : Model -> Html Msg
view model =
  let
    viewSubreddit s =
      let
        viewMultiSub m =
          li
            []
            [ text m.name
            , button
                [ onClick (RemoveFromMulti s m) ]
                [ text "x" ]
            ]
        subMultis =
          let
            folder : MultiredditName -> List Multireddit -> List Multireddit
            folder mname acc =
              case Dict.get mname model.multireddits of
                Nothing -> acc
                Just m  -> m::acc
          in
            Set.foldl folder [] s.multireddits
      in
        li [ classList [ ("subreddit", True)
                       , ("subscribed", s.subscribed) ]
           ] [ text s.link
             , ul [ class "multireddits" ] <| List.map viewMultiSub subMultis
             , button [onClick <| ChooseMulti s] [text "+"]
             , if s.subscribed
               then button [onClick <| Unsubscribe s] [text "unsub"]
               else button [onClick <| Subscribe s] [text "sub"]
             ]
    viewMultireddit m =
      let
        focused =
          case model.state of
            Focus (FMulti mname) -> mname == m.name
            ChoosingMulti s _    -> s.name `Set.member` m.subreddits
            _                    -> False
      in
        li [ classList [ ("multireddit", True)
                       , ("focused", focused) ]
           , onClick <|
               case model.state of
                 ChoosingMulti subreddit returnFocus ->
                   AddToMulti subreddit m returnFocus
                 _ -> SetFocus (FMulti m.name)
           ] [ text m.name ]
    viewOther focus text' =
      li [ onClick (SetFocus focus)
         , classList [ ("focused", model.state == Focus focus) ]
         ] [ text text' ]
    filteredSubreddits =
      case model.state of
        Focus FNone ->
          []
        Focus FAll ->
          Dict.values model.subreddits
        Focus (FMulti mname) ->
          let
            subredditNames =
              model.multireddits
                |> Dict.get mname
                |> Maybe.map .subreddits
                |> Maybe.withDefault Set.empty
            subreddits =
              model.subreddits
                |> Dict.filter (\sname _ -> sname `Set.member` subredditNames)
                |> Dict.values
          in
            subreddits
        Focus FSubscribed ->
          List.filter .subscribed <| Dict.values model.subreddits
        Focus FNotSubscribed ->
          List.filter (not << .subscribed) <| Dict.values model.subreddits
        Focus FNotInMulti ->
          let
            folder _ {subreddits} acc =
              Set.foldl
                    (\item acc' -> Dict.insert item item acc')
                      acc
                      subreddits
            allInMulti =
              Dict.foldl folder Dict.empty model.multireddits

          in
            Dict.merge
                  (\_ s acc -> s::acc)
                  (\_ _ _ acc -> acc)
                  (\_ _ acc -> acc)
                  model.subreddits
                  allInMulti
                  []
        ChoosingMulti subreddit _ ->
          Dict.get subreddit.name model.subreddits |> maybeToList
    menu = ul [ class "menu" ]
           ( [ viewOther FAll "All"
             , viewOther FSubscribed "Subscribed"
             , viewOther FNotSubscribed "Not subscribed"
             , viewOther FNotInMulti "Not in multireddit" ] ++
               (List.map viewMultireddit
                  <| List.sortBy .name
                  <| Dict.values model.multireddits ) )
    content = ul [ class "list" ]
              <| List.map viewSubreddit
              <| List.sortBy .link
              <| filteredSubreddits
  in
    div []
        [ App.map Api (API.view model.apidata)
        , case model.apidata.token of
            Nothing -> text ""
            Just _  -> div [] [
                        button [onClick Load] [text "load"]
                       , menu
                       , content
                       ]
        ]

urlUpdate : Maybe API.Code -> Model -> (Model, Cmd Msg)
urlUpdate code model =
  let (apidata, msg) = API.urlUpdate code model.apidata
  in  { model | apidata = apidata
      } ! [Cmd.map Api msg]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

getSubreddits : Maybe API.Token -> Maybe After -> Cmd Msg
getSubreddits token after =
  let
    url =
      Http.url
      "https://oauth.reddit.com/subreddits/mine/subscriber"
      <| maybeToList (Maybe.map (\a -> ("after", a)) after)
  in
    API.get token GotError GotSubreddits url decodeSubreddits

getMultireddits : Maybe API.Token -> Cmd Msg
getMultireddits token =
  let
    url = Http.url
          "https://oauth.reddit.com/api/multi/mine"
          [("expand_srs", "1")]
  in
    API.get token GotError GotMultireddits url decodeMultireddits

addToMulti : Maybe API.Token -> Subreddit -> Multireddit
           -> Cmd Msg
addToMulti token subreddit multireddit =
  let
    decoder : Json.Decode.Decoder Subreddit
    decoder =
      Json.Decode.customDecoder (at ["name"] Json.Decode.string) <| \decodedName ->
        if decodedName == subreddit.display_name
        then Result.Ok subreddit
        else Result.Err <| "expected \"name\" equal to \"" ++ subreddit.display_name ++ "\", got \"" ++ decodedName ++ "\""
    url = "https://oauth.reddit.com/api/multi" ++ multireddit.link ++ "/r/" ++ subreddit.display_name
    body = Http.string <| String.dropLeft 1 <| Http.url ""
           [ ( "model"
             , Json.Encode.encode 0 <| Json.Encode.object [("name", Json.Encode.string subreddit.display_name)]
             )
           ]
  in
    API.put token GotError (AddedToMulti multireddit) url body decoder

removeFromMulti : Maybe API.Token -> Subreddit -> Multireddit
                -> Cmd Msg
removeFromMulti token subreddit multireddit =
  let
    url = "https://oauth.reddit.com/api/multi" ++ multireddit.link ++ "/r/" ++ subreddit.display_name
  in
    API.delete token GotError (always <| RemovedFromMulti multireddit subreddit) url

subscribe : Maybe API.Token -> Subreddit -> Cmd Msg
subscribe token subreddit =
  let
    body = Http.multipart [ stringData "action" "sub"
                          , stringData "sr" subreddit.name ]
  in
    API.post token GotError Subscribed "https://oauth.reddit.com/api/subscribe"
       body (fromJsonUnit { subreddit | subscribed = True })

unsubscribe : Maybe API.Token -> Subreddit -> Cmd Msg
unsubscribe token subreddit =
  let
    body = Http.multipart [ stringData "action" "unsub"
                          , stringData "sr" subreddit.name ]
  in
    API.post token GotError Unsubscribed "https://oauth.reddit.com/api/subscribe"
       body (fromJsonUnit { subreddit | subscribed = False })
