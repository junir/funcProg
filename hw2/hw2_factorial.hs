module Factorial where

  f :: Integer -> Integer
  f 0 = 1
  f n = if (n > 0)
          then n * f(n-1)
          else error "Digit must be positive"
