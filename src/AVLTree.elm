module AVLTree exposing (Balance(..), Tree, empty, fromList, getBalance, insert, leaf, node, nodeInfo, rotateLeft, rotateRight, updateHeight)


type Tree key val
    = Empty
    | Node (NodeInfo key val)


type alias NodeInfo key val =
    { key : key
    , value : val
    , left : Tree key val
    , right : Tree key val
    , height : Int
    }


type Balance
    = Balanced
    | LeftHeavy Int
    | RightHeavy Int


empty : Tree comparable v
empty =
    Empty


node : NodeInfo comparable v -> Tree comparable v
node =
    Node


nodeInfo : Tree comparable v -> Maybe (NodeInfo comparable v)
nodeInfo tree =
    case tree of
        Empty ->
            Nothing

        Node info ->
            Just info


insert : comparable -> v -> Tree comparable v -> Tree comparable v
insert key value tree =
    let
        newTree =
            case tree of
                Empty ->
                    leaf key value

                Node nod ->
                    if key < nod.key then
                        Node { nod | left = insert key value nod.left }

                    else if key > nod.key then
                        Node { nod | right = insert key value nod.right }

                    else
                        Node { nod | value = value }

        balancedTree =
            case getBalance newTree of
                Balanced ->
                    newTree

                LeftHeavy 1 ->
                    newTree

                RightHeavy 1 ->
                    newTree

                LeftHeavy n ->
                    balanceLeftHeavy newTree

                RightHeavy n ->
                    balanceRightHeavy newTree
    in
    updateHeight balancedTree


leaf : comparable -> v -> Tree comparable v
leaf key value =
    Node { key = key, value = value, left = Empty, right = Empty, height = 0 }


fromList : List ( comparable, v ) -> Tree comparable v
fromList list =
    List.foldl (\( key, value ) tree -> insert key value tree) empty list


height : Tree comparable v -> Int
height tree =
    case tree of
        Empty ->
            -1

        Node nod ->
            nod.height


getBalance : Tree comparable v -> Balance
getBalance tree =
    case tree of
        Empty ->
            Balanced

        Node nod ->
            let
                diff =
                    height nod.left - height nod.right
            in
            if diff > 0 then
                LeftHeavy (abs diff)

            else if diff < 0 then
                RightHeavy (abs diff)

            else
                Balanced


balanceLeftHeavy : Tree comparable v -> Tree comparable v
balanceLeftHeavy tree =
    case tree of
        Empty ->
            Empty

        Node nod ->
            case getBalance nod.left of
                Balanced ->
                    tree

                LeftHeavy n ->
                    rotateRight tree

                RightHeavy n ->
                    let
                        newLeft =
                            rotateLeft nod.left
                    in
                    rotateRight (Node { nod | left = newLeft })


balanceRightHeavy : Tree comparable v -> Tree comparable v
balanceRightHeavy tree =
    case tree of
        Empty ->
            Empty

        Node nod ->
            case getBalance nod.right of
                Balanced ->
                    tree

                RightHeavy n ->
                    rotateLeft tree

                LeftHeavy n ->
                    let
                        newRight =
                            rotateRight nod.right
                    in
                    rotateLeft (Node { nod | right = newRight })


rotateRight : Tree comparable v -> Tree comparable v
rotateRight tree =
    case tree of
        Empty ->
            Empty

        Node nod ->
            case nod.left of
                Empty ->
                    tree

                Node pivotNode ->
                    let
                        newTree =
                            updateHeight (Node { nod | left = pivotNode.right })
                    in
                    Node { pivotNode | right = newTree }


rotateLeft : Tree comparable v -> Tree comparable v
rotateLeft tree =
    case tree of
        Empty ->
            Empty

        Node nod ->
            case nod.right of
                Empty ->
                    tree

                Node pivotNode ->
                    let
                        newTree =
                            updateHeight (Node { nod | right = pivotNode.left })
                    in
                    Node { pivotNode | left = newTree }


updateHeight : Tree comparable v -> Tree comparable v
updateHeight tree =
    case tree of
        Empty ->
            Empty

        Node nod ->
            Node { nod | height = 1 + max (height nod.left) (height nod.right) }
