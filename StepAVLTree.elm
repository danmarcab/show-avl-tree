module StepAVLTree exposing (..)


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


type Msg comparable v
    = StartInsert comparable v
    | InsertRoot comparable v
    | CheckInsert comparable comparable v
    | ChangeValue comparable v
    | InsertLeft comparable comparable v
    | InsertRight comparable comparable v
    | CheckBalance comparable
    | RotateLeft comparable
    | RotateRight comparable
    | Error String


type ApplyableMsg comparable v
    = ApplyChangeValue v
    | ApplyInsertLeft comparable v
    | ApplyInsertRight comparable v
    | ApplyRotateLeft
    | ApplyRotateRight


update : Msg comparable v -> Tree comparable v -> ( Tree comparable v, List (Msg comparable v) )
update msg tree =
    case msg of
        StartInsert key value ->
            case tree of
                EmptyNode ->
                    ( tree, [ InsertRoot key value ] )

                TreeNode node ->
                    ( tree, [ CheckInsert node.key key value ] )

        InsertRoot key value ->
            case tree of
                EmptyNode ->
                    ( leaf key value, [] )

                TreeNode node ->
                    ( tree, [ Error "InsertRoot called in a non empty tree" ] )

        CheckInsert node_key key value ->
            case tree of
                EmptyNode ->
                    ( tree, [ Error "CheckInsert called in an empty tree" ] )

                TreeNode node ->
                    case subTree node_key tree of
                        EmptyNode ->
                            ( tree, [ Error "CheckInsert checking a node that is not in the tree" ] )

                        TreeNode node ->
                            if key < node.key then
                                case node.left of
                                    EmptyNode ->
                                        ( tree, [ InsertLeft node.key key value ] )

                                    TreeNode left ->
                                        ( tree, [ CheckInsert left.key key value ] )
                            else if key > node.key then
                                case node.right of
                                    EmptyNode ->
                                        ( tree, [ InsertRight node.key key value ] )

                                    TreeNode right ->
                                        ( tree, [ CheckInsert right.key key value ] )
                            else
                                ( tree, [ ChangeValue key value ] )

        ChangeValue key value ->
            ( apply key (ApplyChangeValue value) tree, [] )

        InsertLeft node_key key value ->
            ( apply node_key (ApplyInsertLeft key value) tree, [ CheckBalance node_key ] )

        InsertRight node_key key value ->
            ( apply node_key (ApplyInsertRight key value) tree, [ CheckBalance node_key ] )

        CheckBalance key ->
            let
                currentSubTree =
                    subTree key tree

                balanceMsgs =
                    case getBalance currentSubTree of
                        Balanced ->
                            []

                        LeftHeavy 1 ->
                            []

                        RightHeavy 1 ->
                            []

                        LeftHeavy n ->
                            case currentSubTree of
                                EmptyNode ->
                                    [ Error "Empty node on left balance" ]

                                TreeNode node ->
                                    case getBalance node.left of
                                        Balanced ->
                                            []

                                        LeftHeavy n ->
                                            [ RotateRight key ]

                                        RightHeavy n ->
                                            case node.left of
                                                EmptyNode ->
                                                    [ Error "Empty node on left left balance" ]

                                                TreeNode left ->
                                                    [ RotateLeft left.key, RotateRight key ]

                        RightHeavy n ->
                            case currentSubTree of
                                EmptyNode ->
                                    [ Error "Empty node on right balance" ]

                                TreeNode node ->
                                    case getBalance node.right of
                                        Balanced ->
                                            []

                                        RightHeavy n ->
                                            [ RotateLeft key ]

                                        LeftHeavy n ->
                                            case node.right of
                                                EmptyNode ->
                                                    [ Error "Empty node on right right balance" ]

                                                TreeNode right ->
                                                    [ RotateRight right.key, RotateLeft key ]

                parentMsg =
                    case parentKey key tree of
                        Just k ->
                            [ CheckBalance k ]

                        Nothing ->
                            []
            in
                ( tree, balanceMsgs ++ parentMsg )

        RotateLeft key ->
            ( apply key ApplyRotateLeft tree, [] )

        RotateRight key ->
            ( apply key ApplyRotateRight tree, [] )

        Error string ->
            ( tree, [] )


parentKey : comparable -> Tree comparable v -> Maybe comparable
parentKey key tree =
    case tree of
        EmptyNode ->
            Nothing

        TreeNode node ->
            if key < node.key then
                case node.left of
                    EmptyNode ->
                        Nothing

                    TreeNode leftNode ->
                        if key == leftNode.key then
                            Just node.key
                        else
                            parentKey key node.left
            else if key > node.key then
                case node.right of
                    EmptyNode ->
                        Nothing

                    TreeNode rightNode ->
                        if key == rightNode.key then
                            Just node.key
                        else
                            parentKey key node.right
            else
                Nothing


subTree : comparable -> Tree comparable v -> Tree comparable v
subTree key tree =
    case tree of
        EmptyNode ->
            EmptyNode

        TreeNode node ->
            if key < node.key then
                subTree key node.left
            else if key > node.key then
                subTree key node.right
            else
                TreeNode node


apply : comparable -> ApplyableMsg comparable v -> Tree comparable v -> Tree comparable v
apply key msg tree =
    let
        newTree =
            case tree of
                EmptyNode ->
                    EmptyNode

                TreeNode node ->
                    if key < node.key then
                        TreeNode { node | left = apply key msg node.left }
                    else if key > node.key then
                        TreeNode { node | right = apply key msg node.right }
                    else
                        case msg of
                            ApplyChangeValue value ->
                                TreeNode { node | value = value }

                            ApplyInsertLeft key value ->
                                TreeNode { node | left = leaf key value }

                            ApplyInsertRight key value ->
                                TreeNode { node | right = leaf key value }

                            ApplyRotateLeft ->
                                rotateLeft tree

                            ApplyRotateRight ->
                                rotateRight tree
    in
        updateHeight newTree


empty : Tree comparable v
empty =
    EmptyNode


insert : Tree comparable v -> comparable -> v -> Tree comparable v
insert tree key value =
    let
        newTree =
            case tree of
                EmptyNode ->
                    leaf key value

                TreeNode node ->
                    if key < node.key then
                        TreeNode { node | left = insert node.left key value }
                    else if key > node.key then
                        TreeNode { node | right = insert node.right key value }
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
    List.foldl (\( key, value ) tree -> insert tree key value) empty list


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
                        rotateRight (TreeNode { node | left = newLeft })


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
                        rotateLeft (TreeNode { node | right = newRight })


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
