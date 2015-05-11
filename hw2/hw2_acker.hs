module Acker where

  acker :: Integer -> Integer -> Integer
  acker a b = if (a < 0) || (b < 0)
          then error "Digits must be positive"
          else if a == 0
              then b + 1
              else if b == 0
                  then acker (a-1) 1
                  else acker (a- 1) (acker a (b - 1))
