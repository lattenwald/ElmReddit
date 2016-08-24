module Main exposing (..)

import API
import Navigation
import Dict exposing (Dict)
import Html.App as App
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, style)
import Http

import Utils exposing (..)
import Types exposing (..)
import Styles

type Focused = FNone
             | FMulti MultiredditName
             | FAll
             | FSubscribed
             | FNotSubscribed
             | FNotInMulti

type alias Model =
  { apidata      : API.Model
  , subreddits   : Subreddits
  , multireddits : Multireddits
  , focused      : Focused }

model : Model
model =
  { apidata      = API.model
  , subreddits   = Dict.empty
  , multireddits = Dict.empty
  , focused      = FNone }

type Msg = Noop
         | GotError Http.Error
         | Api API.Msg
         | Load
         | GetSubreddits (Maybe After)
         | GotSubreddits (Maybe After, Subreddits)
         | GotMultireddits Multireddits
         | SetFocus Focused

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
    { model | apidata = apidata } ! [Cmd.map Api cmd]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop ->
      model ! []

    GotError err -> -- TODO
      ignore (Debug.log "GotError" err) <|
      model ! [Cmd.none]

    Api API.Loaded ->
      model ! [fire Load]

    Load ->
      model ! [fire (GetSubreddits Nothing)]

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

    GotMultireddits multireddits ->
      let
        folder _ item acc =
          Dict.union acc item.subreddits
        allSubreddits =
          multireddits |> Dict.foldl folder model.subreddits
      in
        { model
        | subreddits = allSubreddits
        , multireddits = multireddits
        } ! [Cmd.none]

    SetFocus focused ->
      { model | focused = focused
      } ! [Cmd.none]

view : Model -> Html Msg
view model =
  let
    viewSubreddit s =
      li [ Styles.item
         , if s.subscribed
           then Styles.subSubscribed
           else Styles.subNotSubscribed
         ] [ text s.link ]
    viewMultireddit m =
      li [ Styles.item
         , onClick (SetFocus <| FMulti m.name)
         , if model.focused == FMulti m.name
           then Styles.focused
           else Styles.empty
         ] [ text m.name ]
    viewOther focus text' =
      li [ Styles.item
         , onClick (SetFocus focus)
         , if model.focused == focus
           then Styles.focused
           else Styles.empty
         ] [ text text' ]
    filteredSubreddits =
      case model.focused of
        FNone ->
          []
        FAll ->
          Dict.values model.subreddits
        FMulti mname ->
          Maybe.withDefault [] <|
          Maybe.map (.subreddits >> Dict.values) <|
          Dict.get mname model.multireddits
        FSubscribed ->
          List.filter .subscribed <| Dict.values model.subreddits
        FNotSubscribed ->
          List.filter (not << .subscribed) <| Dict.values model.subreddits
        FNotInMulti ->
          let
            folder _ {subreddits} acc =
              Dict.foldl
                    (\_ item acc' -> Dict.insert item.name item acc')
                      acc
                      subreddits
            allInMulti =
              Dict.foldl folder Dict.empty model.multireddits
          in
            Dict.values <| Dict.diff model.subreddits allInMulti
    menu = ul [ Styles.list
              , style [("float", "left")] ]
           ( [ viewOther FAll "All"
             , viewOther FSubscribed "Subscribed"
             , viewOther FNotSubscribed "Not subscribed"
             , viewOther FNotInMulti "Not in multireddit" ] ++
               (List.map viewMultireddit
                  <| List.sortBy .name
                  <| Dict.values model.multireddits ) )
    content = ul [ Styles.list
                 , style [ ("paddingLeft", "10em") ] ]
              <| List.map viewSubreddit
              <| List.sortBy .link
              <| filteredSubreddits
  in
    div []
        [ App.map Api (API.view model.apidata)
        , button [onClick Load] [text "load"]
        , menu
        , content
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
