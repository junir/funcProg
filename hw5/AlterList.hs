module AlterList where

data AlterList a b = Nil | Cons a (AlterList b a) deriving (Show, Eq)
-- interp. list with alternating values

empty_list :: AlterList a b
empty_list = Nil

list_with_three_elems :: AlterList Int String
list_with_three_elems = Cons 42 (Cons "234" (Cons 42 empty_list))

list_fn_pattern :: AlterList a b -> c
list_fn_pattern l = case l of
  Nil -> undefined
  Cons type1 l -> undefined a (list_fn_pattern l)

alter_list_length :: AlterList a b -> Int
alter_list_length l = case l of
            Nil -> 0
            Cons a tail -> 1 + (alter_list_length tail)

dmap :: AlterList a b -> (a -> c) -> (b -> d) -> AlterList c d
dmap l func1 func2 = case l of
            Nil -> Nil
            Cons a tail -> Cons (func1 a) (dmap tail func2 func1)
