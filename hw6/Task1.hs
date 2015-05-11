module Task1 where

map_foldl :: (a -> b) -> [a] -> [b]
map_foldl func list = foldl (func_foldl) [] list where
  func_foldl list1 elem = list1 ++ [(func elem)]

map_foldr :: (a -> b) -> [a] -> [b]
map_foldr func list = foldr (func_foldr) [] list where
                            func_foldr elem list1 = [(func elem)] ++ list1
