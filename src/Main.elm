module Main exposing (..)

import AVLTree exposing (..)
import Browser
import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (..)
import Json.Decode
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
    , embedded : Bool
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        embedded =
            Json.Decode.decodeValue (Json.Decode.field "elmUIEmbedded" Json.Decode.bool) flags
                |> Result.withDefault False
    in
    ( { tree = initialTree
      , elem = ""
      , steps = []
      , autorun = False
      , size = ( 600, 400 )
      , embedded = embedded
      }
    , Cmd.none
    )


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
    if autorun then
        Time.every 1000 (always Step)

    else
        Sub.none



-- VIEW --


view : Model -> Html Msg
view model =
    Element.layoutWith
        { options =
            if model.embedded then
                [ Element.noStaticStyleSheet ]

            else
                []
        }
        [ Element.padding 10
        , Font.size 18
        ]
        (Element.column []
            [ menuView model
            , treeView model.size model.steps model.tree
            ]
        )


menuView : Model -> Element Msg
menuView model =
    Element.row [ Element.spacing 30 ]
        [ initView model
        , actionsView model
        , stepView model
        ]


initView : Model -> Element Msg
initView model =
    Element.column
        [ Element.alignTop
        , Element.width (Element.fillPortion 3)
        ]
        [ heading "Initialize the tree"
        , Element.wrappedRow [ Element.spacing 5 ]
            [ actionButton [] ResetToEmpty "Empty"
            , actionButton [] (ResetToRange 7) "Range: 1 - 7"
            , actionButton [] (ResetToRange 15) "Range: 1 - 15"
            , actionButton [] (ResetToRandom 7) "7 random values"
            , actionButton [] (ResetToRandom 15) "15 random values"
            , actionButton [] (ResetToRandom 31) "31 random values"
            ]
        ]


actionsView : Model -> Element Msg
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
    in
    Element.column
        [ Element.alignTop
        , Element.width (Element.fillPortion 4)
        , Element.spacing 10
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing 5
            ]
            [ Input.text
                [ Element.width (Element.px 60)
                , Element.paddingXY 5 3
                ]
                { onChange = UpdateElem
                , text = model.elem
                , placeholder = Nothing
                , label = Input.labelAbove [] (heading "Insert")
                }
            , actionButton [] StepInsert "Step by Step"
            , actionButton [] AutoStepInsert "Auto"
            , actionButton [] Insert "Quick"
            ]
        , heading "Delete"
        , Element.paragraph [] [ Element.text "Coming soon..." ]
        ]


actionButton attrs toMsg title =
    Input.button
        ([ Border.width 1
         , Border.rounded 3
         , Element.paddingXY 5 3
         , Element.width Element.shrink
         , Element.alignBottom
         , Element.mouseOver [ Element.moveUp 1 ]
         ]
            ++ attrs
        )
        { onPress = Just toMsg
        , label =
            Element.text title
        }


stepView : Model -> Element Msg
stepView model =
    let
        content =
            case List.head model.steps of
                Nothing ->
                    [ Element.paragraph []
                        [ Element.text "Nothing to explain. Please start a 'Step by Step' or 'Auto' insert"
                        ]
                    ]

                Just step ->
                    [ Element.paragraph []
                        [ Element.text <| explainStep step
                        ]
                    , actionButton [ Element.alignBottom, Element.alignRight ] Step "Next Step"
                    ]
    in
    Element.column
        [ Element.alignTop
        , Element.width (Element.fillPortion 4)
        , Element.spacing 5
        , Element.height Element.fill
        ]
        ([ heading "Explanation" ]
            ++ content
        )


heading text =
    Element.paragraph
        [ Font.size 22
        , Font.bold
        ]
        [ Element.text text ]


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


treeView : ( Int, Int ) -> List (Step Int Int) -> Tree Int Int -> Element Msg
treeView size steps tree =
    Element.html <| TreeVisualization.view size (List.head steps) tree
