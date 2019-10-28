module Main exposing (..)

import AVLTree exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Ports
import Random
import StepByStepAVLTree exposing (Step(..))
import Time
import TreeVisualization



-- MODEL --


type alias Model =
    { tree : Tree Int Int
    , elem : String
    , steps : List (Step Int Int)
    , autorun : Bool
    , size : ( Int, Int )
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { tree = initialTree, elem = "", steps = [], autorun = False, size = ( 600, 400 ) }, Ports.initSizeInfo () )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initialTree : Tree Int Int
initialTree =
    treeFromList <| List.range 1 7



-- UPDATE --


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateElem val ->
            ( { model | elem = val }, Cmd.none )

        Insert ->
            let
                updater val mod =
                    { mod | tree = AVLTree.insert val val mod.tree }
            in
            ( updateIfValidElem updater model, Cmd.none )

        StepInsert ->
            let
                updater val mod =
                    { mod | steps = [ StartInsert val val ] }
            in
            ( updateIfValidElem updater model, Cmd.none )

        AutoStepInsert ->
            let
                updater val mod =
                    { mod | steps = [ StartInsert val val ], autorun = True }
            in
            ( updateIfValidElem updater model, Cmd.none )

        Step ->
            case model.steps of
                [] ->
                    ( { model | autorun = False }, Cmd.none )

                step :: nextSteps ->
                    let
                        ( newTree, additionalSteps ) =
                            StepByStepAVLTree.applyStep step model.tree
                    in
                    ( { model | tree = newTree, steps = nextSteps ++ additionalSteps }, Cmd.none )

        ResetToEmpty ->
            ( resetModelwithTree model AVLTree.empty, Cmd.none )

        ResetToRange num ->
            ( resetModelwithTree model <| treeFromList <| List.range 1 num, Cmd.none )

        ResetToRandom num ->
            ( resetModelwithTree model AVLTree.empty, Random.generate InitRandomTree (Random.list num <| Random.int -100 100) )

        InitRandomTree list ->
            ( resetModelwithTree model <| treeFromList list, Cmd.none )

        Size size ->
            ( { model | size = size }, Cmd.none )


updateIfValidElem : (Int -> Model -> Model) -> Model -> Model
updateIfValidElem fun model =
    case String.toInt model.elem of
        Just val ->
            fun val { model | elem = "" }

        Nothing ->
            { model | elem = "" }


resetModelwithTree : Model -> Tree Int Int -> Model
resetModelwithTree model newTree =
    { model
        | tree = newTree
        , elem = ""
        , steps = []
        , autorun = False
    }


treeFromList : List Int -> Tree Int Int
treeFromList list =
    AVLTree.fromList (List.map (\n -> ( n, n )) list)



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions { autorun } =
    Sub.batch
        [ if autorun then
            Time.every 1000 (always Step)

          else
            Sub.none
        , Ports.size Size
        ]



-- VIEW --


view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ menuView model
        , div [ style "clear" "both" ] []
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
            case String.toInt model.elem of
                Just _ ->
                    False

                Nothing ->
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
            "We are going to insert " ++ String.fromInt key ++ " in the tree. We'll start from the root."

        InsertRoot key val ->
            "The tree is empty, so We are going to make " ++ String.fromInt key ++ " the root of the tree."

        CheckInsert nodeKey key val ->
            if key > nodeKey then
                String.fromInt key ++ " is greater than " ++ String.fromInt nodeKey ++ ". We need to check the right subtree."

            else if key > nodeKey then
                String.fromInt key ++ " is less than " ++ String.fromInt nodeKey ++ ". We need to check the left subtree."

            else
                String.fromInt key ++ " is equal to " ++ String.fromInt nodeKey ++ ". We found the node."

        ChangeValue key val ->
            "We'll change the value to " ++ String.fromInt val ++ "."

        InsertLeft nodeKey key val ->
            "The left subtree was empty, we'll insert " ++ String.fromInt key ++ " there."

        InsertRight nodeKey key val ->
            "The right subtree was empty, we'll insert " ++ String.fromInt key ++ " there."

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
