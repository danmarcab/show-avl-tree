module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import AVLTree exposing (..)
import Random
import StepByStepAVLTree exposing (Step(..))
import Time exposing (Time)
import TreeVisualization
import Ports


type alias Model =
    { tree : Tree Int Int
    , elem : String
    , steps : List (Step Int Int)
    , autorun : Bool
    , size : ( Int, Int )
    }


type Msg
    = Insert
    | StepInsert
    | AutoStepInsert
    | Step
    | UpdateElem String
    | ResetToEmpty
    | ResetToRange Int
    | ResetToRandom Int
    | InitRandomTree (List Int)
    | Size ( Int, Int )


init : ( Model, Cmd Msg )
init =
    ( { tree = initialTree, elem = "", steps = [], autorun = False, size = ( 600, 400 ) }, Ports.initSizeInfo () )


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

        ResetToEmpty ->
            ( { tree = AVLTree.empty
              , elem = ""
              , steps = []
              , autorun = False
              , size = model.size
              }
            , Cmd.none
            )

        ResetToRange num ->
            ( { tree = AVLTree.fromList (List.map (\n -> ( n, n )) <| List.range 1 num)
              , elem = ""
              , steps = []
              , autorun = False
              , size = model.size
              }
            , Cmd.none
            )

        ResetToRandom num ->
            ( { tree = AVLTree.empty
              , elem = ""
              , steps = []
              , autorun = False
              , size = model.size
              }
            , Random.generate InitRandomTree (Random.list num <| Random.int -100 100)
            )

        InitRandomTree list ->
            ( { model
                | tree = AVLTree.fromList (List.map (\n -> ( n, n )) list)
              }
            , Cmd.none
            )

        Size size ->
            ( { model | size = size }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { autorun } =
    Sub.batch
        [ if autorun then
            Time.every (1 * Time.second) (always Step)
          else
            Sub.none
        , Ports.size Size
        ]


view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ menuView model
        , div [ style [ ( "clear", "both" ) ] ] []
        , treeView model.size model.steps model.tree
        ]


menuView : Model -> Html Msg
menuView model =
    header [ class "row" ]
        [ resetView model
        , actionsView model
        , stepView model
        ]


resetView : Model -> Html Msg
resetView model =
    let
        buttonEnabled =
            List.isEmpty model.steps

        resetButton toMsg title =
            button
                [ type_ "button", onClick toMsg, disabled <| not buttonEnabled, class "dropdown-item" ]
                [ text title ]
    in
        div [ class "col-3" ]
            [ h4 [] [ text "Reset the tree" ]
            , div [ class "dropdown" ]
                [ button
                    [ type_ "button"
                    , disabled <| not buttonEnabled
                    , class "btn btn-warning dropdown-toggle"
                    , attribute "data-toggle" "dropdown"
                    ]
                    [ text "Reset to ..." ]
                , div [ class "dropdown-menu" ]
                    [ resetButton ResetToEmpty "Empty"
                    , resetButton (ResetToRange 7) "Range: 1 - 7"
                    , resetButton (ResetToRange 15) "Range: 1 - 15"
                    , resetButton (ResetToRandom 7) "7 random values"
                    , resetButton (ResetToRandom 15) "15 random values"
                    , resetButton (ResetToRandom 31) "31 random values"
                    ]
                ]
            ]


actionsView : Model -> Html Msg
actionsView model =
    let
        inputsDisabled =
            not <| List.isEmpty model.steps

        invalidElem =
            (model.elem == "-")
                || (model.elem == "+")
                || case String.toInt model.elem of
                    Ok val ->
                        False

                    Err _ ->
                        True

        buttonsDisabled =
            inputsDisabled || invalidElem

        actionButton toMsg title =
            span [ class "input-group-btn" ]
                [ button [ type_ "button", class "btn btn-primary btn-sm", onClick toMsg, disabled buttonsDisabled ]
                    [ text title ]
                ]
    in
        div [ class "col-4" ]
            [ h4 [] [ text "Insert" ]
            , div [ class "input-group" ]
                [ input [ class "form-control", onInput UpdateElem, disabled inputsDisabled, value model.elem ] []
                , actionButton StepInsert "Step by Step"
                , actionButton AutoStepInsert "Auto"
                , actionButton Insert "Quick"
                ]
            , h4 [] [ text "Delete" ]
            , p [] [ text "Coming soon..." ]
            ]


stepView : Model -> Html Msg
stepView model =
    let
        content =
            case List.head model.steps of
                Nothing ->
                    [ text "Nothing to explain. Please start a 'Step by Step' or 'Auto' insert"
                    ]

                Just step ->
                    [ p [] [ text <| explainStep step ]
                    , button [ onClick Step ] [ text "Next Step" ]
                    ]
    in
        div [ class "col" ]
            ([ h4 [] [ text "Explanation" ] ] ++ content)


explainStep : Step Int Int -> String
explainStep step =
    case step of
        StartInsert key val ->
            "We are going to insert " ++ toString key ++ " in the tree. We'll start from the root."

        InsertRoot key val ->
            "The tree is empty, so We are going to make " ++ toString key ++ " the root of the tree."

        CheckInsert nodeKey key val ->
            if key > nodeKey then
                toString key ++ " is greater than " ++ toString nodeKey ++ ". We need to check the right subtree."
            else if key > nodeKey then
                toString key ++ " is less than " ++ toString nodeKey ++ ". We need to check the left subtree."
            else
                toString key ++ " is equal to " ++ toString nodeKey ++ ". We found the node."

        ChangeValue key val ->
            "We'll change the value to " ++ toString val ++ "."

        InsertLeft nodeKey key val ->
            "The left subtree was empty, we'll insert " ++ toString key ++ " there."

        InsertRight nodeKey key val ->
            "The right subtree was empty, we'll insert " ++ toString key ++ " there."

        CheckBalance key ->
            "We check the balance of the subtree."

        RotateLeft key ->
            "The subtree is not balanced, we rotate to the left."

        RotateRight key ->
            "The subtree is not balanced, we rotate to the right."

        Error string ->
            "An error ocurred: " ++ string


treeView : ( Int, Int ) -> List (Step Int Int) -> Tree Int Int -> Html Msg
treeView size steps tree =
    TreeVisualization.view size (List.head steps) tree
