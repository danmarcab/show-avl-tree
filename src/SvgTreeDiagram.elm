module SvgTreeDiagram exposing (..)

{-| Provides a draw function for drawing trees as SVGs.

@docs draw

-}

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SA
import TreeDiagram exposing (..)


svgPosition : Coord -> Svg msg -> Svg msg
svgPosition ( x, y ) svg =
    Svg.g
        [ SA.transform <|
            "translate("
                ++ String.fromFloat x
                ++ " "
                ++ String.fromFloat y
                ++ ")"
        ]
        [ svg ]


svgCompose : Int -> Int -> List (Svg msg) -> Svg msg
svgCompose width height svgs =
    let
        vBox =
            "0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height
    in
    Svg.svg
        [ SA.width "100%"
        , SA.viewBox vBox
        , SA.preserveAspectRatio "xMidYMid meet"
        ]
        [ Svg.g [] svgs ]


svgTransform : Int -> Int -> Coord -> Coord
svgTransform width height coord =
    let
        ( x, y ) =
            coord

        svgX =
            x + (toFloat width / 2)

        svgY =
            (toFloat height / 2) - y
    in
    ( svgX, svgY )


svgDrawable : Drawable (Svg msg) (Html msg)
svgDrawable =
    Drawable svgPosition svgCompose svgTransform


{-| Draws the tree using the provided functions for drawings nodes and edges.
TreeLayout contains some more options for positioning the tree.
-}
draw : TreeLayout -> NodeDrawer a (Svg msg) -> EdgeDrawer (Svg msg) -> Tree a -> Html msg
draw layout drawNode drawLine tree =
    draw_ svgDrawable layout drawNode drawLine tree
