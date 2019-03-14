module Main exposing (main)

import Browser
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import List.Extra exposing (updateIf)
import Maybe.Extra exposing (isJust)
import RemoteData exposing (WebData, toMaybe)
import Requests.Todo as Requests
import Style
import Utils exposing (..)


type Msg
    = NoOp
    | InputTodoField String
    | SubmitTodo String
    | DelTodo Int
    | ToggleTodo Int
    | ToggleFilter
    | ToggleFilterVal
    | GotTodo (WebData (List Todo))
    | PostedTodo Int (WebData Todo)


type alias Todo =
    { id : Int
    , value : String
    , done : Bool
    }


type alias Model =
    { newtodo : String
    , todos : WebData (List Todo)
    , filter : Maybe Bool
    }



-- INIT


initModel : Model
initModel =
    { newtodo = ""
    , todos = RemoteData.NotAsked
    , filter = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Requests.getTodo (RemoteData.fromResult >> GotTodo) initModel.filter
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        InputTodoField newtodo ->
            ( { model | newtodo = newtodo }
            , Cmd.none
            )

        SubmitTodo newvalue ->
            let
                ( newtodos, cmd ) =
                    RemoteData.update addTodo model.todos

                addTodo : List Todo -> ( List Todo, Cmd Msg )
                addTodo todos =
                    let
                        newtodo =
                            { id = uniqueId (List.map .id todos)
                            , value = newvalue
                            , done = False
                            }
                    in
                    ( newtodo :: todos
                    , Requests.postTodo
                        (RemoteData.fromResult >> PostedTodo newtodo.id)
                        (Requests.NewTodo newvalue)
                    )
            in
            ( { model | todos = newtodos, newtodo = "" }
            , cmd
            )

        DelTodo id ->
            let
                ( newtodos, cmd ) =
                    RemoteData.update delTodo model.todos

                delTodo : List Todo -> ( List Todo, Cmd Msg )
                delTodo todos =
                    ( List.filter (\t -> t.id /= id) todos
                    , Requests.deleteTodoByTodoId (\_ -> NoOp) id
                    )
            in
            ( { model | todos = newtodos }
            , cmd
            )

        ToggleFilter ->
            let
                filter =
                    if isJust model.filter then
                        Nothing

                    else
                        Just False
            in
            ( { model | filter = filter }
            , Requests.getTodo (RemoteData.fromResult >> GotTodo) filter
            )

        ToggleFilterVal ->
            let
                filter =
                    Maybe.map not model.filter
            in
            ( { model | filter = filter }
            , Requests.getTodo (RemoteData.fromResult >> GotTodo) filter
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
                            , Requests.putTodoByTodoId (\_ -> NoOp) id todo
                            )

                ( newtodos, cmd ) =
                    RemoteData.update toggleTodo model.todos
            in
            ( { model | todos = newtodos }
            , cmd
            )

        GotTodo response ->
            ( { model | todos = response }
            , Cmd.none
            )

        PostedTodo oldId response ->
            case RemoteData.toMaybe response of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just newtodo ->
                    let
                        ( newtodos, cmd ) =
                            RemoteData.update amendId model.todos

                        amendId : List Todo -> ( List Todo, Cmd Msg )
                        amendId todos =
                            let
                                updateId todo =
                                    { todo | id = newtodo.id }
                            in
                            ( List.Extra.updateIf (\t -> t.id == oldId) updateId todos
                            , Cmd.none
                            )
                    in
                    ( { model | todos = newtodos }
                    , cmd
                    )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , viewInputForm model.newtodo
        , viewFilterForm model.filter
        , div [ Style.container ]
            [ h1 [] [ text "Todo list" ]
            , viewTodoList model.todos
            ]
        , viewFooter
        ]


viewInputForm : String -> Html Msg
viewInputForm newtodo =
    Html.Styled.form
        [ Style.form
        , onSubmit (SubmitTodo newtodo)
        ]
        [ input
            [ Style.form__txt
            , type_ "text"
            , placeholder "todo"
            , value newtodo
            , onInput InputTodoField
            ]
            []
        , button
            [ Style.form__btn, disabled (newtodo == "") ]
            [ text "OK" ]
        ]


viewFilterForm : Maybe Bool -> Html Msg
viewFilterForm filter =
    div []
        [ button
            [ Style.form__btn, onClick ToggleFilter ]
            [ text "Filter" ]
        , case filter of
            Just filterVal ->
                div []
                    [ label [ for "filter" ] [ text "Done" ]
                    , input
                        [ id "filter"
                        , type_ "checkbox"
                        , checked filterVal
                        , onClick ToggleFilterVal
                        ]
                        []
                    ]

            Nothing ->
                text ""
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ Style.nav ]
        [ div [ Style.nav__container ]
            [ div [ Style.nav__title ] [ text "Todo App" ]
            ]
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
