module Utils exposing (delete, get, post, put)

import Http
import Json.Decode as JD exposing (Decoder)
import Types exposing (Token)


put : Maybe Token -> (Http.Error -> msg) -> (data -> msg) -> String -> Http.Body -> Decoder data -> Cmd msg
put maybeToken fail success url body decoder =
    let
        request =
            req "PUT" url maybeToken body (Http.expectJson decoder)

        responseHandler resp =
            case resp of
                Err err ->
                    fail err

                Ok data ->
                    success data
    in
    Http.send responseHandler request


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


delete :
    Maybe Token
    -> (Http.Error -> msg)
    -> msg
    -> String
    -> Cmd msg
delete maybeToken fail success url =
    let
        request =
            req "DELETE" url maybeToken Http.emptyBody Http.expectString

        responseHandler resp =
            case resp of
                Err err ->
                    fail err

                Ok _ ->
                    success
    in
    Http.send responseHandler request


post :
    Maybe Token
    -> (Http.Error -> msg)
    -> (data -> msg)
    -> String
    -> Http.Body
    -> Decoder data
    -> Cmd msg
post maybeToken fail success url body decoder =
    let
        request =
            req "POST" url maybeToken body (Http.expectJson decoder)

        responseHandler resp =
            case resp of
                Err err ->
                    fail err

                Ok data ->
                    success data
    in
    Http.send responseHandler request


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
