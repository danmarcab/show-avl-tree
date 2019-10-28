module StepByStepAVLTree exposing (Step(..), applyStep)

import AVLTree exposing (..)


type Step comparable v
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


applyStep : Step comparable v -> Tree comparable v -> ( Tree comparable v, List (Step comparable v) )
applyStep step tree =
    case step of
        StartInsert key value ->
            case nodeInfo tree of
                Nothing ->
                    ( tree, [ InsertRoot key value ] )

                Just node ->
                    ( tree, [ CheckInsert node.key key value ] )

        InsertRoot key value ->
            case nodeInfo tree of
                Nothing ->
                    ( leaf key value, [] )

                Just node ->
                    ( tree, [ Error "InsertRoot called in a non empty tree" ] )

        CheckInsert node_key key value ->
            case nodeInfo tree of
                Nothing ->
                    ( tree, [ Error "CheckInsert called in an empty tree" ] )

                Just node ->
                    case nodeInfo <| subTree node_key tree of
                        Nothing ->
                            ( tree, [ Error "CheckInsert checking a node that is not in the tree" ] )

                        Just info ->
                            if key < info.key then
                                case nodeInfo info.left of
                                    Nothing ->
                                        ( tree, [ InsertLeft info.key key value ] )

                                    Just left ->
                                        ( tree, [ CheckInsert left.key key value ] )

                            else if key > info.key then
                                case nodeInfo info.right of
                                    Nothing ->
                                        ( tree, [ InsertRight info.key key value ] )

                                    Just right ->
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

                        LeftHeavy _ ->
                            case nodeInfo currentSubTree of
                                Nothing ->
                                    [ Error "Empty node on left balance" ]

                                Just node ->
                                    case getBalance node.left of
                                        Balanced ->
                                            []

                                        LeftHeavy _ ->
                                            [ RotateRight key ]

                                        RightHeavy _ ->
                                            case nodeInfo node.left of
                                                Nothing ->
                                                    [ Error "Empty node on left left balance" ]

                                                Just left ->
                                                    [ RotateLeft left.key, RotateRight key ]

                        RightHeavy _ ->
                            case nodeInfo currentSubTree of
                                Nothing ->
                                    [ Error "Empty node on right balance" ]

                                Just node ->
                                    case getBalance node.right of
                                        Balanced ->
                                            []

                                        RightHeavy _ ->
                                            [ RotateLeft key ]

                                        LeftHeavy _ ->
                                            case nodeInfo node.right of
                                                Nothing ->
                                                    [ Error "Empty node on right right balance" ]

                                                Just right ->
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
    case nodeInfo tree of
        Nothing ->
            Nothing

        Just node ->
            if key < node.key then
                case nodeInfo node.left of
                    Nothing ->
                        Nothing

                    Just leftNode ->
                        if key == leftNode.key then
                            Just node.key

                        else
                            parentKey key node.left

            else if key > node.key then
                case nodeInfo node.right of
                    Nothing ->
                        Nothing

                    Just rightNode ->
                        if key == rightNode.key then
                            Just node.key

                        else
                            parentKey key node.right

            else
                Nothing


subTree : comparable -> Tree comparable v -> Tree comparable v
subTree key tree =
    case nodeInfo tree of
        Nothing ->
            empty

        Just node ->
            if key < node.key then
                subTree key node.left

            else if key > node.key then
                subTree key node.right

            else
                tree


type ApplyableStep comparable v
    = ApplyChangeValue v
    | ApplyInsertLeft comparable v
    | ApplyInsertRight comparable v
    | ApplyRotateLeft
    | ApplyRotateRight


apply : comparable -> ApplyableStep comparable v -> Tree comparable v -> Tree comparable v
apply key step tree =
    let
        newTree =
            case nodeInfo tree of
                Nothing ->
                    empty

                Just node ->
                    if key < node.key then
                        AVLTree.node { node | left = apply key step node.left }

                    else if key > node.key then
                        AVLTree.node { node | right = apply key step node.right }

                    else
                        case step of
                            ApplyChangeValue value ->
                                AVLTree.node { node | value = value }

                            ApplyInsertLeft stepKey value ->
                                AVLTree.node { node | left = leaf stepKey value }

                            ApplyInsertRight stepKey value ->
                                AVLTree.node { node | right = leaf stepKey value }

                            ApplyRotateLeft ->
                                rotateLeft tree

                            ApplyRotateRight ->
                                rotateRight tree
    in
    updateHeight newTree
