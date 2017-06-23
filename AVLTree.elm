module AVLTree exposing (..)


type Tree comparable v
    = EmptyNode
    | TreeNode (Node comparable v)


type alias Node comparable v =
    { key : comparable
    , value : v
    , left : Tree comparable v
    , right : Tree comparable v
    , height : Int
    }


type Balance
    = Balanced
    | LeftHeavy Int
    | RightHeavy Int


empty : Tree comparable v
empty =
    EmptyNode


insert : comparable -> v -> Tree comparable v ->  Tree comparable v
insert key value tree =
    let
        newTree =
            case tree of
                EmptyNode ->
                    leaf key value

                TreeNode node ->
                    if key < node.key then
                        TreeNode { node | left = insert key value node.left}
                    else if key > node.key then
                        TreeNode { node | right = insert key value node.right}
                    else
                        TreeNode { node | value = value }

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
    TreeNode (Node key value EmptyNode EmptyNode 0)


fromList : List ( comparable, v ) -> Tree comparable v
fromList list =
    List.foldl (\( key, value ) tree -> insert key value tree) empty list


height : Tree comparable v -> Int
height tree =
    case tree of
        EmptyNode ->
            -1

        TreeNode node ->
            node.height


getBalance : Tree comparable v -> Balance
getBalance tree =
    case tree of
        EmptyNode ->
            Balanced

        TreeNode node ->
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
        EmptyNode ->
            EmptyNode

        TreeNode node ->
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
                        rotateRight (TreeNode {node | left = newLeft})


balanceRightHeavy : Tree comparable v -> Tree comparable v
balanceRightHeavy tree =
    case tree of
        EmptyNode ->
            EmptyNode

        TreeNode node ->
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
                        rotateLeft (TreeNode {node | right = newRight})


rotateRight : Tree comparable v -> Tree comparable v
rotateRight tree =
    case tree of
        EmptyNode ->
            EmptyNode

        TreeNode node ->
            case node.left of
                EmptyNode ->
                    tree

                TreeNode pivotNode ->
                    let
                        newTree =
                            updateHeight (TreeNode { node | left = pivotNode.right })
                    in
                        (TreeNode { pivotNode | right = newTree })


rotateLeft : Tree comparable v -> Tree comparable v
rotateLeft tree =
    case tree of
        EmptyNode ->
            EmptyNode

        TreeNode node ->
            case node.right of
                EmptyNode ->
                    tree

                TreeNode pivotNode ->
                    let
                        newTree =
                            updateHeight (TreeNode { node | right = pivotNode.left })
                    in
                        (TreeNode { pivotNode | left = newTree })


updateHeight : Tree comparable v -> Tree comparable v
updateHeight tree =
    case tree of
        EmptyNode ->
            EmptyNode

        TreeNode node ->
            TreeNode { node | height = 1 + (max (height node.left) (height node.right)) }
