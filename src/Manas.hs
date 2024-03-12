module Manas where

import Definition


empT :: T
empT = T{_shape=NoShape,_size=0,_hardness=0,_power=0,_element=A}

empY :: Y
empY = Y Be []

empMn :: Mana
empMn = Mana empT empY

mBStick :: [Arg] -> Mana
mBStick args = Mana T{_shape=Pole,_size=30,_hardness=40,_power=10,_element=I}
                  $ Y Be args

mVHit :: [Arg] -> Mana
mVHit args = Mana  empT $ Y Hit args

stick0 :: Int -> Mana
stick0 i = mBStick [Arg South (mASize empT i)]

hitWithStick0 :: Mana
hitWithStick0 = mVHit [Arg South (stick0 40)] 

mASize :: T -> Int -> Mana
mASize t i = Mana t{_size=i} empY 

mAHardness :: T -> Int -> Mana
mAHardness t i = Mana t{_hardness=i} empY 

mAPower :: T -> Int -> Mana
mAPower t i = Mana t{_power=i} empY 

mAElement :: T -> Element -> Mana
mAElement t e = Mana t{_element=e} empY 

chT :: T -> T -> T
chT (T i0 i1 i2 i3 i4) (T t0 t1 t2 t3 t4) = 
  let (T e0 e1 e2 e3 e4) = empT
      rs i t e = if t==e then i else t
   in T (rs i0 t0 e0) (rs i1 t1 e1) (rs i2 t2 e2) (rs i3 t3 e3) (rs i4 t4 e4)   

