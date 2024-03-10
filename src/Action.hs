{-# LANGUAGE OverloadedStrings #-}
module Action where

import Linear.V2 (V2(..))
import Object (getPosByName,getLayerByName,getOprByPos,getLayerByPos,updatePosByName)
import Definition (Pos,Input(..),MapWhole,MapObject,MapCell(..),ObProperty(..))

type MapPos = Pos
type MapWinPos = Pos
type IsDiagonal = Bool

movePlayer :: Input -> IsDiagonal -> MapWinPos -> MapPos -> MapWhole
                                   -> MapObject -> (MapObject,MapPos) 
movePlayer p b (V2 w h) (V2 mx my) md mo =  
  let pps = getPosByName "player" mo
      ply = getLayerByName "player" mo
      dps = case p of
         Ri -> if b then V2 1 (-1) else V2 1 0
         Up -> if b then V2 (-1) (-1) else V2 0 (-1)
         Lf -> if b then V2 (-1) 1 else V2 (-1) 0
         Dn -> if b then V2 1 1 else V2 0 1
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
      nps@(V2 nx ny) = if isInMap && not isBlock then tps else pps
      mw' = fromIntegral mw; mh' = fromIntegral mh
      nmx 
        | nx-mx < 1 && mx > 0 = mx - 1 
        | nx-mx > w-1 && mx < mw'-w = mx + 1
        | otherwise = mx
      nmy 
        | ny-my < 1 && my > 0 = my - 1
        | ny-my > h-1 && my < mh'-h = my + 1
        | otherwise = my
      nmo = if nps/=pps then updatePosByName "player" nps mo else mo
   in (nmo, V2 nmx nmy)
      

