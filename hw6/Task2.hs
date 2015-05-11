module Task2 where

--
--Filter
--

filter_rec :: (a -> Bool) -> [a] -> [a]
filter_rec func [] = []
filter_rec func (x:xs) = if func x
                  then x : (filter_rec func xs)
                  else (filter_rec func xs)

filter_foldl :: (a -> Bool) -> [a] -> [a]
filter_foldl func list = foldl (func_foldl_filter ) [] list where
                            func_foldl_filter list1 elem = if func elem
                                                      then list1 ++ [elem]
                                                      else list1

filter_foldr :: (a -> Bool) -> [a] -> [a]
filter_foldr func list = foldr (func_foldr_filter) [] list where
                              func_foldr_filter elem list1 = if func elem
                                                          then [elem] ++ list1
                                                          else list1

--
--Concat
--

concat_rec:: [[a]] -> [a]
concat_rec [] = []
concat_rec (x:xs) = x ++ concat_rec xs

concat_foldl :: [[a]] -> [a]
concat_foldl list = foldl (func_foldl_concat) [] list where
                          func_foldl_concat list1 elem = list1 ++ elem

concat_foldr :: [[a]] -> [a]
concat_foldr list = foldr (func_for_foldr_concat) [] list where
                          func_for_foldr_concat elem list1 = elem ++ list1

--
--ConcatMap
--

concatMap_rec :: (a -> [b]) -> [a] -> [b]
concatMap_rec func [] = []
concatMap_rec func (x:xs) = (func x) ++ (concatMap_rec func xs)

concatMap_foldl :: (a -> [b]) -> [a] -> [b]
concatMap_foldl func list = foldl (func_foldl_concatMap) [] list where
                            func_foldl_concatMap list1 elem = list1 ++ func elem

concatMap_foldr :: (a -> [b]) -> [a] -> [b]
concatMap_foldr func list = foldr (func_foldr_concatMap) [] list where
                            func_foldr_concatMap elem list1 = func elem ++ list1
