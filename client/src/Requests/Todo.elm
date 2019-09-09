module Requests.Todo exposing (NewTodo, Todo, deleteTodoByTodoId, getTodo, getTodoByTodoId, jsonDecNewTodo, jsonDecTodo, jsonEncNewTodo, jsonEncTodo, postTodo, putTodoByTodoId)

-- The following module comes from bartavelle/json-helpers

import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode exposing (Value)
import Json.Helpers exposing (..)
import Set
import String
import Url.Builder


type alias Todo =
    { id : String
    , value : String
    , done : Bool
    }


jsonDecTodo : Json.Decode.Decoder Todo
jsonDecTodo =
    Json.Decode.succeed (\pid pvalue pdone -> { id = pid, value = pvalue, done = pdone })
        |> required "id" Json.Decode.string
        |> required "value" Json.Decode.string
        |> required "done" Json.Decode.bool


jsonEncTodo : Todo -> Value
jsonEncTodo val =
    Json.Encode.object
        [ ( "id", Json.Encode.string val.id )
        , ( "value", Json.Encode.string val.value )
        , ( "done", Json.Encode.bool val.done )
        ]


type alias NewTodo =
    { value : String
    }


jsonDecNewTodo : Json.Decode.Decoder NewTodo
jsonDecNewTodo =
    Json.Decode.succeed (\pvalue -> { value = pvalue }) |> custom Json.Decode.string


jsonEncNewTodo : NewTodo -> Value
jsonEncNewTodo val =
    Json.Encode.string val.value


getTodo : Maybe Bool -> (Result Http.Error (List Todo) -> msg) -> Cmd msg
getTodo query_done toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    [ [ query_done
                            |> Maybe.map
                                ((\value ->
                                    if value then
                                        "true"

                                    else
                                        "false"
                                 )
                                    >> Url.Builder.string "done"
                                )
                      ]
                    ]
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:3030"
                [ "todo"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (Json.Decode.list jsonDecTodo)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getTodoByTodoId : String -> (Result Http.Error (Maybe Todo) -> msg) -> Cmd msg
getTodoByTodoId capture_todoId toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:3030"
                [ "todo"
                , capture_todoId
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (Json.Decode.maybe jsonDecTodo)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postTodo : NewTodo -> (Result Http.Error (Maybe Todo) -> msg) -> Cmd msg
postTodo body toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:3030"
                [ "todo"
                ]
                params
        , body =
            Http.jsonBody (jsonEncNewTodo body)
        , expect =
            Http.expectJson toMsg (Json.Decode.maybe jsonDecTodo)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


deleteTodoByTodoId : String -> (Result Http.Error () -> msg) -> Cmd msg
deleteTodoByTodoId capture_todoId toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:3030"
                [ "todo"
                , capture_todoId
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectString
                (\x ->
                    case x of
                        Err e ->
                            toMsg (Err e)

                        Ok _ ->
                            toMsg (Ok ())
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


putTodoByTodoId : String -> Todo -> (Result Http.Error () -> msg) -> Cmd msg
putTodoByTodoId capture_todoId body toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:3030"
                [ "todo"
                , capture_todoId
                ]
                params
        , body =
            Http.jsonBody (jsonEncTodo body)
        , expect =
            Http.expectString
                (\x ->
                    case x of
                        Err e ->
                            toMsg (Err e)

                        Ok _ ->
                            toMsg (Ok ())
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
