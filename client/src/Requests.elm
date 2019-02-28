module Requests exposing (NewTodo, NoContent(..), Todo, decodeTodo, deleteTodoByTodoId, encodeNewTodo, encodeTodo, getTodo, getTodoByTodoId, postTodo, putTodoByTodoId)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String


type NoContent
    = NoContent


type alias Todo =
    { id : Int
    , value : String
    , done : Bool
    }


type alias NewTodo =
    { value : String
    }


decodeTodo : Decoder Todo
decodeTodo =
    succeed Todo
        |> required "id" int
        |> required "value" string
        |> required "done" bool


encodeTodo : Todo -> Json.Encode.Value
encodeTodo x =
    Json.Encode.object
        [ ( "id", Json.Encode.int x.id )
        , ( "value", Json.Encode.string x.value )
        , ( "done", Json.Encode.bool x.done )
        ]


encodeNewTodo : NewTodo -> Json.Encode.Value
encodeNewTodo x =
    Json.Encode.object
        [ ( "value", Json.Encode.string x.value )
        ]


getTodo : (Result ( Maybe ( Http.Metadata, String ), Http.Error ) (List Todo) -> msg) -> Cmd msg
getTodo toMsg =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:3030"
                , "todo"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url ->
                            Err ( Nothing, Http.BadUrl url )

                        Http.Timeout_ ->
                            Err ( Nothing, Http.Timeout )

                        Http.NetworkError_ ->
                            Err ( Nothing, Http.NetworkError )

                        Http.BadStatus_ metadata body_ ->
                            Err ( Just ( metadata, body_ ), Http.BadStatus metadata.statusCode )

                        Http.GoodStatus_ metadata body_ ->
                            decodeString (list decodeTodo) body_
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just ( metadata, body_ )))
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getTodoByTodoId : (Result ( Maybe ( Http.Metadata, String ), Http.Error ) (Maybe Todo) -> msg) -> Int -> Cmd msg
getTodoByTodoId toMsg capture_todoId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:3030"
                , "todo"
                , capture_todoId |> String.fromInt |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url ->
                            Err ( Nothing, Http.BadUrl url )

                        Http.Timeout_ ->
                            Err ( Nothing, Http.Timeout )

                        Http.NetworkError_ ->
                            Err ( Nothing, Http.NetworkError )

                        Http.BadStatus_ metadata body_ ->
                            Err ( Just ( metadata, body_ ), Http.BadStatus metadata.statusCode )

                        Http.GoodStatus_ metadata body_ ->
                            decodeString (maybe decodeTodo) body_
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just ( metadata, body_ )))
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postTodo : (Result ( Maybe ( Http.Metadata, String ), Http.Error ) Todo -> msg) -> NewTodo -> Cmd msg
postTodo toMsg body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:3030"
                , "todo"
                ]
        , body =
            Http.jsonBody (encodeNewTodo body)
        , expect =
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url ->
                            Err ( Nothing, Http.BadUrl url )

                        Http.Timeout_ ->
                            Err ( Nothing, Http.Timeout )

                        Http.NetworkError_ ->
                            Err ( Nothing, Http.NetworkError )

                        Http.BadStatus_ metadata body_ ->
                            Err ( Just ( metadata, body_ ), Http.BadStatus metadata.statusCode )

                        Http.GoodStatus_ metadata body_ ->
                            decodeString decodeTodo body_
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just ( metadata, body_ )))
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


deleteTodoByTodoId : (Result ( Maybe ( Http.Metadata, String ), Http.Error ) NoContent -> msg) -> Int -> Cmd msg
deleteTodoByTodoId toMsg capture_todoId =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:3030"
                , "todo"
                , capture_todoId |> String.fromInt |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url ->
                            Err ( Nothing, Http.BadUrl url )

                        Http.Timeout_ ->
                            Err ( Nothing, Http.Timeout )

                        Http.NetworkError_ ->
                            Err ( Nothing, Http.NetworkError )

                        Http.BadStatus_ metadata body_ ->
                            Err ( Just ( metadata, body_ ), Http.BadStatus metadata.statusCode )

                        Http.GoodStatus_ metadata body_ ->
                            if String.isEmpty body_ then
                                Ok NoContent

                            else
                                Err ( Just ( metadata, body_ ), Http.BadBody <| "Expected the response body to be empty, but it was '" ++ body_ ++ "'." )
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


putTodoByTodoId : (Result ( Maybe ( Http.Metadata, String ), Http.Error ) NoContent -> msg) -> Int -> Todo -> Cmd msg
putTodoByTodoId toMsg capture_todoId body =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:3030"
                , "todo"
                , capture_todoId |> String.fromInt |> Url.percentEncode
                ]
        , body =
            Http.jsonBody (encodeTodo body)
        , expect =
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url ->
                            Err ( Nothing, Http.BadUrl url )

                        Http.Timeout_ ->
                            Err ( Nothing, Http.Timeout )

                        Http.NetworkError_ ->
                            Err ( Nothing, Http.NetworkError )

                        Http.BadStatus_ metadata body_ ->
                            Err ( Just ( metadata, body_ ), Http.BadStatus metadata.statusCode )

                        Http.GoodStatus_ metadata body_ ->
                            if String.isEmpty body_ then
                                Ok NoContent

                            else
                                Err ( Just ( metadata, body_ ), Http.BadBody <| "Expected the response body to be empty, but it was '" ++ body_ ++ "'." )
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
