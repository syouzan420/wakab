module Manas where

import Definition


empT :: T
empT = T{_shape=NoShape,_size=0,_hardness=0,_power=0,_element=A}

mBStick :: Arg -> Mana
mBStick arg = Mana T{_shape=Pole,_size=30,_hardness=40,_power=10,_element=I}
                 $ Y Be arg

mVHit :: Arg -> Mana
mVHit arg = Mana  empT $ Y Hit arg
