module YesItCanFly where

data DinnerOrder = Chicken | Pasta | NoFood deriving (Show, Eq)
--inerp. dinner variety

dinner_order :: DinnerOrder -> a
dinner_order d = case d of
              Chicken -> undefined
              Pasta -> undefined
              NoFood -> undefined

dinner_order_to_msg :: DinnerOrder -> String
dinner_order_to_msg dm = case dm of
              Chicken -> "Chicken."
              Pasta -> "Pasta."
              NoFood -> "NoFood."
