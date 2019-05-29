module Requests.Todo exposing (NewTodo, NoContent(..), Todo, decodeTodo, deleteTodoByTodoId, encodeNewTodo, encodeTodo, getTodo, getTodoByTodoId, postTodo, putTodoByTodoId)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Maybe.Extra
import String
import Url.Builder


type NoContent
    = NoContent


type alias Todo =
    { id : String
    , value : String
    , done : Bool
    }


type alias NewTodo =
    { value : String
    }


decodeTodo : Decoder Todo
decodeTodo =
    Json.Decode.succeed Todo
        |> required "id" string
        |> required "value" string
        |> required "done" bool


encodeTodo : Todo -> Json.Encode.Value
encodeTodo x =
    Json.Encode.object
        [ ( "id", Json.Encode.string x.id )
        , ( "value", Json.Encode.string x.value )
        , ( "done", Json.Encode.bool x.done )
        ]


encodeNewTodo : NewTodo -> Json.Encode.Value
encodeNewTodo x =
    Json.Encode.object
        [ ( "value", Json.Encode.string x.value )
        ]


getTodo : (Result Http.Error (List Todo) -> msg) -> Maybe Bool -> Cmd msg
getTodo toMsg query_done =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:3030"
                [ "todo"
                ]
                (List.concat
                    [ query_done
                        |> Maybe.Extra.toList
                        |> List.map
                            (\v ->
                                if v then
                                    "True"

                                else
                                    "False"
                            )
                        |> List.map (Url.Builder.string "done")
                    ]
                )
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (list decodeTodo)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getTodoByTodoId : (Result Http.Error (Maybe Todo) -> msg) -> String -> Cmd msg
getTodoByTodoId toMsg capture_todoId =
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
                []
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (nullable decodeTodo)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postTodo : (Result Http.Error (Maybe Todo) -> msg) -> NewTodo -> Cmd msg
postTodo toMsg body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:3030"
                [ "todo"
                ]
                []
        , body =
            Http.jsonBody (encodeNewTodo body)
        , expect =
            Http.expectJson toMsg (nullable decodeTodo)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


deleteTodoByTodoId : (Result Http.Error NoContent -> msg) -> String -> Cmd msg
deleteTodoByTodoId toMsg capture_todoId =
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
                []
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse toMsg
                (\response ->
                    case response of
                        Http.GoodStatus_ _ "" ->
                            Ok NoContent

                        _ ->
                            Err (Http.BadBody "Expected the response body to be empty")
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


putTodoByTodoId : (Result Http.Error NoContent -> msg) -> String -> Todo -> Cmd msg
putTodoByTodoId toMsg capture_todoId body =
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
                []
        , body =
            Http.jsonBody (encodeTodo body)
        , expect =
            Http.expectStringResponse toMsg
                (\response ->
                    case response of
                        Http.GoodStatus_ _ "" ->
                            Ok NoContent

                        _ ->
                            Err (Http.BadBody "Expected the response body to be empty")
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
