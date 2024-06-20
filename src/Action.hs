{-# LANGUAGE OverloadedStrings #-}
module Action (movePlayer,hitAction) where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Object (getPosByName,getLayerByName,getObjByPos,updatePosByName)
import Converter (inpToDir,dirToDelta)
import Definition (Pos,Input(..),MapWhole,MapObject,MapCell(..),PEvent(..)
                  ,Object(..),ObProperty(..),ObName,Chra(..),Direction(..)
                  ,EvAct(..))

type MapSize = (Int,Int)
type MapPos = Pos
type PlyPos = Pos
type MapWinPos = Pos
type IsDiagonal = Bool

movePlayer :: Input -> IsDiagonal -> MapWinPos -> MapPos -> MapWhole
                                   -> MapObject -> (MapObject,MapPos,PlyPos,[PEvent]) 
movePlayer p b (V2 w h) (V2 mx my) md mo =  
  let pps = getPosByName "player" mo
      ply = getLayerByName "player" mo
      dps = dirToDelta $ inpToDir b p
      tps@(V2 tx ty) = pps + dps
      tx' = fromIntegral tx; ty' = fromIntegral ty
      mh = length md
      mw = if null md then 0 else length (head md)
      isInMap = tx'>=0 && tx'<mw && ty'>=0 && ty'<mh 
      mcot = (md!!ty')!!tx' -- map cell on target
      obsl@(Ob _ _ _ _ soPr) = getObjByPos tps ply mo  -- map opr on target
      obdl@(Ob _ _ _ _ doPr) = getObjByPos tps (ply-1) mo
      obul@(Ob _ _ _ _ uoPr) = getObjByPos tps (ply+1) mo
      isFace = soPr `elem` [Ch,Bl,Mv]
      isRide = doPr /= No
      isHide = uoPr /= No
      isBlock = (mcot `elem` [Wall,Block,Water]) || isFace
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
      evt = [PMove nps]<>[PFace obsl | isFace]<>[PRide obdl | isRide]
                       <>[PHide obul | isHide]
   in (nmo, V2 nmx nmy, nps, evt)


---not yet
checkAct :: EvAct -> PEvent -> Int 
checkAct (EvAct evi _ co) pe =
  let evis = T.splitOn "." evi
   in if length evis==4 then  
        let (act:tp:cont:num:_) = evis
            isAct = case act of
              "ride" -> case pe of
                          PRide (Ob ch _ _ _ _) -> case tp of
                                                        "ch" -> cont==T.pack [ch] 
                                                        _ -> False
                          _ -> False
              _ -> False
         in if isAct then if co+1==(read . T.unpack) num then (-1) else co+1
                     else co 
                        else co

hitAction :: ObName -> [Chra] -> MapSize -> MapObject -> MapObject 
hitAction onm chras (mh,mw) mt = 
  let tchra = filter (\(Chra nm _ _ _) -> nm==onm) chras 
      (Chra _ ps dr hn) = 
          if null tchra then Chra T.empty (V2 (-1) 0) South (Nothing,Nothing)
                        else head tchra 
      eps@(V2 ex ey) = ps + dirToDelta dr
      isShow = ex>=0 && ex<fromIntegral mw && ey>=0 && ey<fromIntegral mh
   in if isShow then Ob '/' "hit" 1 eps Ef:mt else mt 
