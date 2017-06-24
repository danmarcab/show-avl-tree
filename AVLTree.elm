module AVLTree exposing (Tree, empty, node, nodeInfo, insert, leaf, fromList, Balance(..), getBalance, updateHeight, rotateLeft, rotateRight)


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

                Node node ->
                    if key < node.key then
                        Node { node | left = insert key value node.left }
                    else if key > node.key then
                        Node { node | right = insert key value node.right }
                    else
                        Node { node | value = value }

        balancedTree =
            case getBalance newTree of
                Balanced ->
                    newTree

                LeftHeavy 1 ->
                    newTree

                RightHeavy 1 ->
                    newTree

                LeftHeavy n ->
                    balanceLeftHeavy (newTree)

                RightHeavy n ->
                    balanceRightHeavy (newTree)
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

        Node node ->
            node.height


getBalance : Tree comparable v -> Balance
getBalance tree =
    case tree of
        Empty ->
            Balanced

        Node node ->
            let
                diff =
                    (height node.left) - (height node.right)
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

        Node node ->
            case getBalance node.left of
                Balanced ->
                    tree

                LeftHeavy n ->
                    rotateRight tree

                RightHeavy n ->
                    let
                        newLeft =
                            rotateLeft node.left
                    in
                        rotateRight (Node { node | left = newLeft })


balanceRightHeavy : Tree comparable v -> Tree comparable v
balanceRightHeavy tree =
    case tree of
        Empty ->
            Empty

        Node node ->
            case getBalance node.right of
                Balanced ->
                    tree

                RightHeavy n ->
                    rotateLeft tree

                LeftHeavy n ->
                    let
                        newRight =
                            rotateRight node.right
                    in
                        rotateLeft (Node { node | right = newRight })


rotateRight : Tree comparable v -> Tree comparable v
rotateRight tree =
    case tree of
        Empty ->
            Empty

        Node node ->
            case node.left of
                Empty ->
                    tree

                Node pivotNode ->
                    let
                        newTree =
                            updateHeight (Node { node | left = pivotNode.right })
                    in
                        (Node { pivotNode | right = newTree })


rotateLeft : Tree comparable v -> Tree comparable v
rotateLeft tree =
    case tree of
        Empty ->
            Empty

        Node node ->
            case node.right of
                Empty ->
                    tree

                Node pivotNode ->
                    let
                        newTree =
                            updateHeight (Node { node | right = pivotNode.left })
                    in
                        (Node { pivotNode | left = newTree })


updateHeight : Tree comparable v -> Tree comparable v
updateHeight tree =
    case tree of
        Empty ->
            Empty

        Node node ->
            Node { node | height = 1 + (max (height node.left) (height node.right)) }
