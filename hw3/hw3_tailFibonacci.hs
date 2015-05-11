module hw3_tailFibonacci where

  tailF :: Integer -> Integer

  tailF a
    | a == 0 = 0
    | otherwise = tailFib a 1 0
            where
              tailFib 0 result previous = result
              tailFib n result previous = tailFib (a - 1) (result + previous) result
