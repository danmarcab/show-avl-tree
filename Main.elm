module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import StepAVLTree exposing (..)
import Time exposing (Time)


type alias Model =
    { tree : Tree Int Int
    , elem : String
    , steps : List (StepAVLTree.Msg Int Int)
    , autorun : Bool
    }


type Msg
    = Insert
    | StepInsert
    | AutoStepInsert
    | Step
    | UpdateElem String


init : ( Model, Cmd Msg )
init =
    ( { tree = initialTree, elem = "", steps = [], autorun = False }, Cmd.none )


initialTree : Tree Int Int
initialTree =
    StepAVLTree.fromList [ ( 5, 5 ), ( 4, 4 ), ( 6, 6 ), ( 1, 1 ), ( 2, 2 ), ( 10, 10 ), ( 7, 7 ), ( 8, 8 ), ( 3, 3 ) ]


main =
    program { init = init, view = view, update = update, subscriptions = subscriptions }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateElem val ->
            ( { model | elem = val }, Cmd.none )

        Insert ->
            let
                newModel =
                    case String.toInt model.elem of
                        Ok val ->
                            { model | tree = StepAVLTree.insert model.tree val val, elem = "" }

                        Err _ ->
                            { model | elem = "" }
            in
                ( newModel, Cmd.none )

        StepInsert ->
            let
                newModel =
                    case String.toInt model.elem of
                        Ok val ->
                            { model | steps = [ StartInsert val val ] }

                        Err _ ->
                            { model | elem = "" }
            in
                ( newModel, Cmd.none )

        AutoStepInsert ->
            let
                newModel =
                    case String.toInt model.elem of
                        Ok val ->
                            { model | steps = [ StartInsert val val ] }

                        Err _ ->
                            { model | elem = "" }
            in
                ( { newModel | autorun = True }, Cmd.none )

        Step ->
            case model.steps of
                [] ->
                    ( { model | autorun = False }, Cmd.none )

                step :: steps ->
                    let
                        ( newTree, nextSteps ) =
                            StepAVLTree.update step model.tree

                        newSteps =
                            steps ++ nextSteps
                    in
                        ( { model | tree = newTree, steps = newSteps, elem = "" }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { autorun } =
    if autorun then
        Time.every (1 * Time.second) (always Step)
    else
        Sub.none


view : Model -> Html Msg
view model =
    div
        []
        [ menuView model, treeTopView model.steps model.tree ]


menuView : Model -> Html Msg
menuView model =
    case model.steps of
        [] ->
            div []
                [ input [ onInput UpdateElem, value model.elem ] []
                , button [ onClick Insert ] [ text "Quick Insert" ]
                , button [ onClick StepInsert ] [ text "Manual Step by Step Insert" ]
                , button [ onClick AutoStepInsert ] [ text "Auto Step by Step Insert" ]
                ]

        step :: steps ->
            div []
                [ text ("Next Step " ++ (toString step))
                , button [ onClick Step ] [ text "Advance One Step" ]
                ]


treeTopView : List (StepAVLTree.Msg Int Int) -> Tree Int Int -> Html Msg
treeTopView steps tree =
    let
        step =
            case steps of
                [] ->
                    Nothing

                step :: rest ->
                    Just step
    in
        div [ class "tree" ]
            [ ul []
                [ li [] (treeView step tree) ]
            ]


treeView : Maybe (StepAVLTree.Msg Int Int) -> Tree Int Int -> List (Html Msg)
treeView maybeStep tree =
    let
        (node_key, nodeClass) =
            case maybeStep of
                Nothing ->
                    (-1, "")

                Just (CheckInsert nodeKey key value) ->
                    (nodeKey, "check-node")

                Just (CheckBalance nodeKey) ->
                    (nodeKey, "check-node")

                Just (ChangeValue nodeKey value) ->
                    (nodeKey, "check-node")

                Just (InsertLeft nodeKey key value) ->
                    (nodeKey, "insert-left")

                Just (InsertRight nodeKey key value) ->
                    (nodeKey, "insert-right")

                Just (RotateLeft nodeKey) ->
                    (nodeKey, "insert-left")

                Just (RotateRight nodeKey) ->
                    (nodeKey, "insert-right")

                Just _ ->
                    (-1, "")
    in
        case tree of
            EmptyNode ->
                [ div [] [ text "-" ] ]

            TreeNode node ->
                [ div [classList [(nodeClass, node.key == node_key)]]
                    [ span [] [ text ("key: " ++ (toString node.key)) ]
                    , br [] []
                    , span [] [ text ("h: " ++ (toString node.height)) ]
                    , br [] []
                    , span [] [ text ("bal: " ++ (toString (StepAVLTree.getBalance tree))) ]
                    ]
                , ul
                    []
                    [ li [] (treeView maybeStep node.left)
                    , li [] (treeView maybeStep node.right)
                    ]
                ]



--    = StartInsert comparable v
--    | InsertRoot comparable v
--    | CheckInsert comparable comparable v
--    | ChangeValue comparable v
--    | InsertLeft comparable comparable v
--    | InsertRight comparable comparable v
--    | CheckBalance comparable
--    | RotateLeft comparable
--    | RotateRight comparable
--    | Error String
