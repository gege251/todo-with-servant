module Main exposing (main)

import Browser
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import List.Extra exposing (updateIf)
import Maybe.Extra as MaybeE
import RemoteData exposing (WebData, toMaybe)
import Requests.Todo as Requests
import Style
import Utils exposing (..)


type Msg
    = NoOp
    | InputTodoField String
    | SubmitTodo
    | ToggleFilter
    | RemoveFilter
    | DelTodo String
    | ToggleTodo String
    | GotTodos (WebData (List Todo))
    | PostedTodo (WebData (Maybe Todo))


type alias Todo =
    { id : String
    , value : String
    , done : Bool
    }


type alias Model =
    { newTodo : String
    , todos : WebData (List Todo)
    , filter : Maybe Bool
    }



-- INIT


initModel : Model
initModel =
    { newTodo = ""
    , todos = RemoteData.NotAsked
    , filter = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Requests.getTodo Nothing (RemoteData.fromResult >> GotTodos)
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        InputTodoField newTodo ->
            ( { model | newTodo = newTodo }
            , Cmd.none
            )

        SubmitTodo ->
            ( { model | newTodo = "" }
            , Requests.postTodo
                { value = model.newTodo }
                (RemoteData.fromResult >> PostedTodo)
            )

        ToggleFilter ->
            let
                newFilter =
                    case model.filter of
                        Nothing ->
                            Just False

                        Just filter ->
                            Just (not filter)
            in
            ( { model | filter = newFilter }
            , Requests.getTodo newFilter (RemoteData.fromResult >> GotTodos)
            )

        RemoveFilter ->
            ( { model | filter = Nothing }
            , Requests.getTodo Nothing (RemoteData.fromResult >> GotTodos)
            )

        DelTodo id ->
            let
                ( newTodos, cmd ) =
                    RemoteData.update delTodo model.todos

                delTodo : List Todo -> ( List Todo, Cmd Msg )
                delTodo todos =
                    ( List.filter (\t -> t.id /= id) todos
                    , Requests.deleteTodoByTodoId id (\_ -> NoOp)
                    )
            in
            ( { model | todos = newTodos }
            , cmd
            )

        ToggleTodo id ->
            let
                toggleTodo : List Todo -> ( List Todo, Cmd Msg )
                toggleTodo todos =
                    let
                        maybeTodo =
                            List.Extra.find (\t -> t.id == id) todos
                                |> Maybe.map (\t -> { t | done = not t.done })
                    in
                    case maybeTodo of
                        Nothing ->
                            ( todos
                            , Cmd.none
                            )

                        Just todo ->
                            ( List.Extra.setIf (\t -> t.id == id) todo todos
                            , Requests.putTodoByTodoId id todo (\_ -> NoOp)
                            )

                ( newTodos, cmd ) =
                    RemoteData.update toggleTodo model.todos
            in
            ( { model | todos = newTodos }
            , cmd
            )

        GotTodos response ->
            ( { model | todos = response }
            , Cmd.none
            )

        PostedTodo response ->
            let
                appendTodo maybeNewTodo todos =
                    List.append (MaybeE.toList maybeNewTodo) todos
            in
            ( { model | todos = RemoteData.map2 appendTodo response model.todos }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , viewForm model.newTodo
        , button [ Style.form__btn, onClick ToggleFilter ]
            [ text "Toggle filter" ]
        , button [ Style.form__btn, onClick RemoveFilter, disabled (model.filter == Nothing) ]
            [ text "Remove filter" ]
        , div [ Style.container ]
            [ h1 [] [ text "Todo list" ]
            , viewTodoList model.todos
            ]
        , viewFooter
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ Style.nav ]
        [ div [ Style.nav__container ]
            [ div [ Style.nav__title ] [ text "Todo App" ] ]
        ]


viewForm : String -> Html Msg
viewForm newTodo =
    Html.Styled.form
        [ Style.form
        , onSubmit SubmitTodo
        ]
        [ input
            [ Style.form__txt
            , type_ "text"
            , placeholder "Create a new task"
            , value newTodo
            , onInput InputTodoField
            ]
            []
        , button
            [ Style.form__btn, disabled (newTodo == "") ]
            [ text "OK" ]
        ]


viewTodoList : WebData (List Todo) -> Html Msg
viewTodoList todos =
    case todos of
        RemoteData.NotAsked ->
            text "Initializing..."

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success list ->
            case List.length list of
                0 ->
                    text "No items."

                otherwise ->
                    ul [] (List.map viewListElem list)

        RemoteData.Failure error ->
            text "Could not fetch todos."


viewListElem : Todo -> Html Msg
viewListElem todo =
    li [ Style.todo__li ]
        [ input
            [ Style.todo__chkbox
            , type_ "checkbox"
            , checked todo.done
            , onClick (ToggleTodo todo.id)
            ]
            []
        , div [] [ text todo.value ]
        , button
            [ Style.form__btn
            , Style.flex__right
            , onClick (DelTodo todo.id)
            , disabled (not todo.done)
            ]
            [ text "Delete" ]
        ]


viewFooter : Html Msg
viewFooter =
    div [ Style.footer ]
        [ text "This is a nice footer." ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = always Sub.none
        }
