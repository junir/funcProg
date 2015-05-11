module Kasha where

type KashaTemp = Int
-- interp. the kasha temperature. temperature could be in [0..20]

low, normal, high :: KashaTemp

low = 0
normal = 10
high = 20

kasha_temp :: KashaTemp -> a
kasha_temp temp | 0 <= temp && temp <= 20 = undefined temp


data Cooker = GoLeft | GoRight | StayCurr deriving (Show,Eq)
-- interp. the cooker

fn_cooker :: Cooker -> a
fn_cooker c = case c of
								  GoLeft -> undefined
								  GoRight -> undefined
								  StayCurr -> undefined

kasha_temp_to_cooker :: KashaTemp -> Cooker
kasha_temp_to_cooker temp = if 0 <= temp && temp <= 20
										then if temp > normal
									  then GoLeft
									  else if temp < normal
										then GoRight
										else StayCurr
										else error "temperature is wrong"
