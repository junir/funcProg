module BinaryTree where

data BinaryTree a = Leaf a | Branch (BinaryTree a) (BinaryTree a) deriving(Show)
--interp. binary tree that contains some type elems in leafs

tree_with_two_elems :: BinaryTree Int
tree_with_two_elems = Branch (Leaf 1) (Leaf 10)

tree_fn_pattern :: BinaryTree a -> b
tree_fn_pattern t = case t of
        Leaf a -> undefined a
        Branch l r -> (tree_fn_pattern l) (tree_fn_pattern r)

tree_height :: BinaryTree a -> Int
tree_height t = case t of
        Leaf a -> 1
        Branch l r-> 1 + max(tree_height l) (tree_height r)

tmap :: BinaryTree a -> (a -> b) -> BinaryTree b
tmap t func = case t of
        Leaf a -> Leaf (func a)
        Branch l r -> Branch (tmap l func) (tmap r func)
