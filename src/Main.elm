module Main exposing (..)

-- TODOOO features:
-- TODO create new multi
-- TODO remove empty multi

import API
import Navigation
import Dict exposing (Dict)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, style, class, classList)
import Http
import Json.Encode
import Json.Decode exposing (at)
import Set exposing (Set)
import String exposing (join)
import Utils exposing (..)
import Types exposing (..)

import Html5.DragDrop as DragDrop

type Focused
  = FNone
  | FMulti MultiredditName
  | FAll
  | FSubscribed
  | FNotSubscribed
  | FNotInMulti


type alias Model =
  { apidata      : API.Model
  , subreddits   : Subreddits
  , multireddits : Multireddits
  , focus        : Focused
  , dragDrop     : DragDrop.Model SubredditName MultiredditName
  }


emptyModel : Model
emptyModel =
  { apidata      = API.model
  , subreddits   = Dict.empty
  , multireddits = Dict.empty
  , focus        = FAll
  , dragDrop     = DragDrop.init
  }


type Msg
  = Noop
  | GotError Http.Error
  | Api API.Msg
  | Load
  | GetSubreddits (Maybe After)
  | GotSubreddits ( Maybe After, Subreddits )
  | GotMultireddits ( Multireddits, Subreddits )
  | SetFocus Focused
  | AddToMulti Subreddit Multireddit Focused
  | AddedToMulti Multireddit Subreddit
  | RemoveFromMulti Subreddit Multireddit
  | RemovedFromMulti Multireddit Subreddit
  | Subscribe Subreddit
  | Subscribed Subreddit
  | Unsubscribe Subreddit
  | Unsubscribed Subreddit
  | DragDropMsg (DragDrop.Msg SubredditName MultiredditName)


main =
  Navigation.program (API.urlParser >> Api)
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init loc =
  let
    ( apidata, cmd ) =
      API.init loc
  in
    { emptyModel | apidata = apidata } ! [ Cmd.map Api cmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Noop ->
      model ! []

    GotError err ->
      -- TODO show this somewhere but console
      ignore (Debug.log "GotError" err) <|
        model
          ! [ Cmd.none ]

    Api (API.Loaded) ->
      model ! [ fire Load ]

    Load ->
      { model
        | subreddits = Dict.empty
        , multireddits = Dict.empty
      }
        ! [ fire (GetSubreddits Nothing) ]

    Api msg ->
      let
        ( apidata, apimsg ) =
          API.update msg model.apidata
      in
        { model
          | apidata = apidata
        }
          ! [ Cmd.map Api apimsg ]

    GetSubreddits after ->
      model ! [ getSubreddits model.apidata.token after ]

    GotSubreddits ( after, subreddits ) ->
      { model
        | subreddits = Dict.union subreddits model.subreddits
      }
        ! [ case after of
              Nothing ->
                getMultireddits model.apidata.token

              Just a ->
                getSubreddits model.apidata.token after
          ]

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
            folder mname { subreddits } acc =
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
                Set.foldl folder_ acc subreddits
          in
            Dict.foldl folder Dict.empty multireddits

        newSubreddits : Subreddits
        newSubreddits =
          let
            subWithMultis =
              Dict.map
                (\sname s ->
                  { s
                    | multireddits =
                        Maybe.withDefault
                          Set.empty
                          (Dict.get sname subMultis)
                  }
                )
          in
            subWithMultis <| Dict.union subreddits model.subreddits
      in
        { model
          | subreddits = newSubreddits
          , multireddits = multireddits
        }
          ! [ Cmd.none ]

    SetFocus focused ->
      { model | focus = focused } ! [ Cmd.none ]

    AddToMulti subreddit multireddit returnFocus ->
      { model | focus = returnFocus
      } ! [ addToMulti model.apidata.token subreddit multireddit ]

    AddedToMulti m s ->
      let
        newSub =
          (\sub -> { sub | multireddits = Set.insert m.name sub.multireddits }) <|
            Maybe.withDefault s <|
              Dict.get s.name model.subreddits

        newMulti =
          (\multi -> { multi | subreddits = Set.insert s.name multi.subreddits }) <|
            Maybe.withDefault m <|
              Dict.get m.name model.multireddits
      in
        { model
        | subreddits = Dict.insert newSub.name newSub model.subreddits
        , multireddits = Dict.insert newMulti.name newMulti model.multireddits
        } ! [ Cmd.none ]

    RemoveFromMulti s m ->
      model ! [ removeFromMulti model.apidata.token s m ]

    RemovedFromMulti m s ->
      let
        subUpdater : Maybe Subreddit -> Maybe Subreddit
        subUpdater =
          Maybe.map (\s -> { s | multireddits = Set.remove m.name s.multireddits })

        multiUpdater : Maybe Multireddit -> Maybe Multireddit
        multiUpdater =
          Maybe.map (\m -> { m | subreddits = Set.remove s.name m.subreddits })
      in
        { model
          | subreddits = Dict.update s.name subUpdater model.subreddits
          , multireddits = Dict.update m.name multiUpdater model.multireddits
        }
          ! [ Cmd.none ]

    Subscribe s ->
      model ! [ subscribe model.apidata.token s ]

    Subscribed s ->
      { model
        | subreddits = Dict.insert s.name s model.subreddits
      }
        ! [ Cmd.none ]

    Unsubscribe s ->
      model ! [ unsubscribe model.apidata.token s ]

    Unsubscribed s ->
      { model
        | subreddits = Dict.insert s.name s model.subreddits
      }
        ! [ Cmd.none ]

    DragDropMsg ddmsg ->
      let (ddmodel, result) = DragDrop.update ddmsg model.dragDrop
          cmds = result
               |> Maybe.andThen (\ (sname, mname) ->
                                   Maybe.map2 (,) (Dict.get sname model.subreddits) (Dict.get mname model.multireddits))
               |> Maybe.map (\(s, m) -> [fire (AddToMulti s m model.focus)])
               |> Maybe.withDefault []
      in
        { model | dragDrop = ddmodel } ! cmds


view : Model -> Html Msg
view model =
  let
    dropId = DragDrop.getDropId model.dragDrop

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
                Nothing ->
                  acc

                Just m ->
                  m :: acc
          in
            Set.foldl folder [] s.multireddits
      in
        li
          ([ classList
               [ ( "subreddit", True )
               , ( "subscribed", s.subscribed )
               ]
           ] ++ DragDrop.draggable DragDropMsg s.name)
          [ a [ href <| redditLink s.link ] [ text s.link ]
          , ul [ class "multireddits" ] <| List.map viewMultiSub subMultis
          , if s.subscribed then
              button [ onClick <| Unsubscribe s ] [ text "unsub" ]
            else
              button [ onClick <| Subscribe s ] [ text "sub" ]
          ]

    viewMultireddit m =
      let
        focused =
          case model.focus of
            FMulti mname ->
              mname == m.name

            _ ->
              False

        dropHover = dropId |> Maybe.map ((==) m.name) |> Maybe.withDefault False

        link =
          redditLink <|
            "/r/"
              ++ join "+"
                  (model.subreddits
                    |> Dict.filter (\name _ -> Set.member name m.subreddits)
                    |> Dict.map (\_ { display_name } -> display_name)
                    |> Dict.values
                  )
      in
        li
          ([ classList
               [ ( "multireddit", True )
               , ( "focused", focused )
               , ( "dropHover", dropHover )
               ]
           ] ++ DragDrop.droppable DragDropMsg m.name)
          [ a [ href link ] [ text "-> " ]
          , span
              [ onClick <| SetFocus (FMulti m.name) ]
              [ text m.name ]
          ]

    viewOther focus text_ =
      li
        [ onClick (SetFocus focus)
        , classList [ ( "focused", model.focus == focus ) ]
        ]
        [ text text_ ]

    filteredSubreddits =
      case model.focus of
        FNone ->
          []

        FAll ->
          Dict.values model.subreddits

        FMulti mname ->
          let
            subredditNames =
              model.multireddits
                |> Dict.get mname
                |> Maybe.map .subreddits
                |> Maybe.withDefault Set.empty

            subreddits =
              model.subreddits
                |> Dict.filter (\sname _ -> Set.member sname subredditNames)
                |> Dict.values
          in
            subreddits

        FSubscribed ->
          List.filter .subscribed <| Dict.values model.subreddits

        FNotSubscribed ->
          List.filter (not << .subscribed) <| Dict.values model.subreddits

        FNotInMulti ->
          let
            folder _ { subreddits } acc =
              Set.foldl
                (\item acc_ -> Dict.insert item item acc_)
                acc
                subreddits

            allInMulti =
              Dict.foldl folder Dict.empty model.multireddits
          in
            Dict.merge
              (\_ s acc -> s :: acc)
              (\_ _ _ acc -> acc)
              (\_ _ acc -> acc)
              model.subreddits
              allInMulti
              []

    menu =
      ul [ class "menu" ]
        ([ viewOther FAll "All"
         , viewOther FSubscribed "Subscribed"
         , viewOther FNotSubscribed "Not subscribed"
         , viewOther FNotInMulti "Not in multireddit"
         ]
          ++ (List.map viewMultireddit <|
                List.sortBy .name <|
                  Dict.values model.multireddits
             )
        )

    content =
      ul [ class "list" ] <|
        List.map viewSubreddit <|
          List.sortBy .link <|
            filteredSubreddits
  in
    div []
      [ Html.map Api (API.view model.apidata)
      , case model.apidata.token of
          Nothing ->
            text ""

          Just _ ->
            div []
              [ button [ onClick Load ] [ text "load" ]
              , menu
              , content
              ]
      ]


urlUpdate : Maybe API.Code -> Model -> ( Model, Cmd Msg )
urlUpdate code model =
  let
    ( apidata, msg ) =
      API.urlUpdate code model.apidata
  in
    { model
      | apidata = apidata
    }
      ! [ Cmd.map Api msg ]


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


getSubreddits : Maybe API.Token -> Maybe After -> Cmd Msg
getSubreddits token after =
  let
    url_ =
      url
        "https://oauth.reddit.com/subreddits/mine/subscriber"
      <|
        maybeToList (Maybe.map (\a -> ( "after", a )) after)
  in
    API.get token GotError GotSubreddits url_ decodeSubreddits


getMultireddits : Maybe API.Token -> Cmd Msg
getMultireddits token =
  let
    url_ =
      url
        "https://oauth.reddit.com/api/multi/mine"
        [ ( "expand_srs", "1" ) ]
  in
    API.get token GotError GotMultireddits url_ decodeMultireddits


addToMulti :
  Maybe API.Token
  -> Subreddit
  -> Multireddit
  -> Cmd Msg
addToMulti token subreddit multireddit =
  let
    decoder : Json.Decode.Decoder Subreddit
    decoder =
      "name" := Json.Decode.string |> Json.Decode.andThen
                (\decodedName ->
                   if decodedName == subreddit.display_name
                   then Json.Decode.succeed subreddit
                   else Json.Decode.fail <| "expected \"name\" equal to \"" ++ subreddit.display_name ++ "\", got \"" ++ decodedName ++ "\"")

    url =
      "https://oauth.reddit.com/api/multi" ++ multireddit.link ++ "/r/" ++ subreddit.display_name

    body =
      Http.multipartBody [ Http.stringPart "model" <| Json.Encode.encode 0 <| Json.Encode.object [ ( "name", Json.Encode.string subreddit.display_name ) ] ]
  in
    API.put token GotError (AddedToMulti multireddit) url body decoder

removeFromMulti :
  Maybe API.Token
  -> Subreddit
  -> Multireddit
  -> Cmd Msg
removeFromMulti token subreddit multireddit =
  let
    url =
      "https://oauth.reddit.com/api/multi" ++ multireddit.link ++ "/r/" ++ subreddit.display_name
  in
    API.delete token GotError (RemovedFromMulti multireddit subreddit) url


subscribe : Maybe API.Token -> Subreddit -> Cmd Msg
subscribe token subreddit =
  let
    body =
      Http.multipartBody
        [ Http.stringPart "action" "sub"
        , Http.stringPart "sr" subreddit.name
        ]
  in
    API.post token
      GotError
      Subscribed
      "https://oauth.reddit.com/api/subscribe"
      body
      (fromJsonUnit { subreddit | subscribed = True })


unsubscribe : Maybe API.Token -> Subreddit -> Cmd Msg
unsubscribe token subreddit =
  let
    body =
      Http.multipartBody
        [ Http.stringPart "action" "unsub"
        , Http.stringPart "sr" subreddit.name
        ]
  in
    API.post token
      GotError
      Unsubscribed
      "https://oauth.reddit.com/api/subscribe"
      body
      (fromJsonUnit { subreddit | subscribed = False })


redditLink : String -> String
redditLink link =
  "https://www.reddit.com" ++ link
