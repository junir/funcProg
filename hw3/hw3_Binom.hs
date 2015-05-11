module hw3_Binom where

  binom :: Integer -> Integer -> Integer
  binom a b = binom' 1 1 a b
      where
        binom' c d a 0 = c `div` d
        binom' c d 0 b = 0
        binom' c d a b = binom' (c * a) (p * b) (a-1) (b-1)
