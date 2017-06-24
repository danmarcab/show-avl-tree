module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import AVLTree exposing (..)
import StepByStepAVLTree exposing (Step(..))
import Time exposing (Time)
import TreeVisualization


type alias Model =
    { tree : Tree Int Int
    , elem : String
    , steps : List (Step Int Int)
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
    AVLTree.fromList (List.map (\n -> ( n, n )) <| List.range 1 7)


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
                            { model | tree = AVLTree.insert val val model.tree, elem = "" }

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
                            StepByStepAVLTree.applyStep step model.tree

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
        [ menuView model
        , treeView model.steps model.tree
        ]


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


treeView : List (Step Int Int) -> Tree Int Int -> Html Msg
treeView steps tree =
    TreeVisualization.view (List.head steps) tree
