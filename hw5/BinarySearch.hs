module BinarySearch where

data BinarySearchTree = Nil | Cons Int BinarySearchTree BinarySearchTree deriving(Show)
-- interp. binary search tree

empty_tree :: BinarySearchTree
empty_tree = Nil

tree_with_two_elems :: BinarySearchTree
tree_with_two_elems = Cons 3 (Cons 2 empty_tree empty_tree) empty_tree

tree_fn_pattern :: BinarySearchTree -> a
tree_fn_pattern t = case t of
		Nil -> undefined
		Cons value l r -> undefined value (tree_fn_pattern l) (tree_fn_pattern r)

tree_height :: BinarySearchTree -> Int
tree_height tree = case tree of
	Nil -> 0
	Cons v l r -> 1 + max (tree_height l) (tree_height r)

tree_sum :: BinarySearchTree -> Int
tree_sum t = case t of
	Nil -> 0
	Cons v l r -> v + (tree_sum l) + (tree_sum r)

tree_search :: BinarySearchTree -> Int -> Bool
tree_search t val = case t of
	Nil -> False
	Cons v l r -> if v == val
							  then True
						      else if val < v
								then tree_search l val
							    else tree_search r val
