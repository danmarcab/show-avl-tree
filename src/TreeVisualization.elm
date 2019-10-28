module TreeVisualization exposing (view)

import AVLTree exposing (..)
import Html exposing (Html)
import StepByStepAVLTree exposing (Step(..))
import Svg exposing (Svg, circle, g, line, polygon, rect, svg, text, text_)
import Svg.Attributes exposing (..)
import TreeDiagram exposing (defaultTreeLayout)
import TreeDiagram.Svg


view : ( Int, Int ) -> Maybe (Step Int Int) -> Tree Int Int -> Html msg
view ( w, h ) maybeStep tree =
    let
        vBox =
            "0 0 " ++ String.fromInt w ++ " " ++ String.fromInt h
    in
    Svg.svg
        [ width "100%"
        , height "600"
        , viewBox vBox
        , preserveAspectRatio "xMidYMid meet"
        , class "show-avl-tree-wrapper"
        ]
        [ TreeDiagram.Svg.draw treeLayout drawNode (drawEdge "black") <| toTree maybeStep tree
        ]


treeLayout =
    { defaultTreeLayout | padding = 60, siblingDistance = 80 }


drawEdge : String -> ( Float, Float ) -> Svg msg
drawEdge color ( x, y ) =
    let
        arrowOffset =
            42

        theta =
            atan (y / x)

        rot_ =
            if x > 0 then
                theta

            else
                pi + theta

        rot =
            (rot_ / (2 * pi)) * 360

        dist =
            sqrt (x ^ 2 + y ^ 2)

        scale =
            (dist - arrowOffset) / dist

        ( xTo, yTo ) =
            ( scale * x, scale * y )
    in
    g
        []
        [ line
            [ x1 (String.fromInt 0)
            , y1 (String.fromInt 0)
            , x2 (String.fromFloat xTo)
            , y2 (String.fromFloat yTo)
            , stroke color
            , strokeWidth "2"
            ]
            []
        , g
            [ transform <|
                "translate("
                    ++ String.fromFloat xTo
                    ++ " "
                    ++ String.fromFloat yTo
                    ++ ") "
                    ++ "rotate("
                    ++ String.fromFloat rot
                    ++ ")"
            ]
            [ arrow color ]
        ]


arrow : String -> Svg msg
arrow color =
    polygon [ points "-10,10 10,0, -10,-10 -5,0", stroke color, fill color ] []


type Node a
    = Empty
    | Normal a
    | GoLeft a
    | GoRight a
    | Found a
    | Change a
    | RotLeft a
    | RotRight a
    | CheckBal a


drawNode : Node Int -> Svg msg
drawNode node =
    case node of
        Empty ->
            emptyNode

        Normal n ->
            simpleNode "black" "white" n

        GoLeft n ->
            complexNode "black" "green" n <| [ leftArrow "green" ]

        GoRight n ->
            complexNode "black" "green" n <| [ rightArrow "green" ]

        Found n ->
            simpleNode "black" "red" n

        Change n ->
            simpleNode "black" "red" n

        RotLeft n ->
            complexNode "black" "blue" n <| [ rotateLeftArrows "blue" ]

        RotRight n ->
            complexNode "black" "blue" n <| [ rotateRightArrows "blue" ]

        CheckBal n ->
            simpleNode "black" "purple" n


emptyNode : Svg msg
emptyNode =
    rect [ width "25", height "25", stroke "black", strokeWidth "2", fill "white", transform "translate(-12.5,-25)" ] []


simpleNode : String -> String -> Int -> Svg msg
simpleNode textColor backColor value =
    complexNode textColor backColor value []


complexNode : String -> String -> Int -> List (Svg msg) -> Svg msg
complexNode textColor backColor value extraSvg =
    g
        []
        ([ circle [ r "27", stroke "black", strokeWidth "2", fill backColor, cx "0", cy "0" ] []
         , text_ [ textAnchor "middle", fill textColor, fontSize "30", fontFamily "sans-serif", transform "translate(0,11)" ]
            [ text <| String.fromInt value ]
         ]
            ++ extraSvg
        )


leftArrow : String -> Svg msg
leftArrow color =
    g
        [ transform "translate(-30,0)" ]
        [ drawEdge color ( -60, 60 )
        ]


rightArrow : String -> Svg msg
rightArrow color =
    g
        [ transform "translate(30,0)" ]
        [ drawEdge color ( 60, 60 )
        ]


rotateLeftArrows : String -> Svg msg
rotateLeftArrows color =
    g []
        [ leftArrow color
        , g
            [ transform "translate(70,45)" ]
            [ drawEdge color ( -60, -60 )
            ]
        ]


rotateRightArrows : String -> Svg msg
rotateRightArrows color =
    g []
        [ rightArrow color
        , g
            [ transform "translate(-70,45)" ]
            [ drawEdge color ( 60, -60 )
            ]
        ]


toTree : Maybe (Step Int Int) -> Tree Int Int -> TreeDiagram.Tree (Node Int)
toTree maybeStep tree =
    case AVLTree.nodeInfo tree of
        Nothing ->
            TreeDiagram.node Empty []

        Just node ->
            TreeDiagram.node (buildNode maybeStep node.key) (List.map (toTree maybeStep) [ node.left, node.right ])


buildNode : Maybe (Step Int Int) -> Int -> Node Int
buildNode maybeStep key =
    case maybeStep of
        Nothing ->
            Normal key

        Just step ->
            case step of
                StartInsert k v ->
                    Normal key

                InsertRoot k v ->
                    Normal key

                CheckInsert k1 k2 v ->
                    if k1 == key then
                        if k2 < key then
                            GoLeft key

                        else if k2 > key then
                            GoRight key

                        else
                            Found key

                    else
                        Normal key

                ChangeValue k v ->
                    if k == key then
                        Change key

                    else
                        Normal key

                InsertLeft k1 k2 v ->
                    if k1 == key then
                        GoLeft key

                    else
                        Normal key

                InsertRight k1 k2 v ->
                    if k1 == key then
                        GoRight key

                    else
                        Normal key

                CheckBalance k ->
                    if k == key then
                        CheckBal key

                    else
                        Normal key

                RotateLeft k ->
                    if k == key then
                        RotLeft key

                    else
                        Normal key

                RotateRight k ->
                    if k == key then
                        RotRight key

                    else
                        Normal key

                _ ->
                    Normal key
