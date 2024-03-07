{-# LANGUAGE OverloadedStrings #-}
module Action where

import Linear.V2 (V2(..))
import Object (getPosByName,getLayerByName,getOprByPos,getLayerByPos,updatePosByName)
import Definition (Pos,Input(..),MapWhole,MapObject,MapCell(..),ObProperty(..))

movePlayer :: Input -> MapWhole -> MapObject -> MapObject 
movePlayer p md mo =  
  let pps = getPosByName "player" mo
      ply = getLayerByName "player" mo
      dps = case p of
         Ri -> V2 1 0
         Up -> V2 0 (-1)
         Lf -> V2 (-1) 0
         Dn -> V2 0 1
         _ -> V2 0 0
      tps@(V2 tx ty) = pps + dps
      tx' = fromIntegral tx; ty' = fromIntegral ty
      mh = length md
      mw = if null md then 0 else length (head md)
      isInMap = tx'>=0 && tx'<mw && ty'>=0 && ty'<mh 
      mcot = (md!!ty')!!tx' -- map cell on target
      moot = getOprByPos tps mo  -- map opr on target
      molot = getLayerByPos tps mo -- map object layer on target
      isBlock = (mcot `elem` [Wall,Block,Water]) || 
                ((moot `elem` [Ch,Bl,Mv]) && molot==ply) 
      nps = if isInMap && not isBlock then tps else pps
      nmo = if nps/=pps then updatePosByName "player" nps mo else mo
   in nmo
      

