module Object where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Definition (Pos,ObName,ObLayer,MapObject,Object(..),ObProperty(..))

getPosByName :: ObName -> MapObject -> Pos
getPosByName _ [] = V2 (-1) (-1)
getPosByName tn ((Ob _ nm _ ps _):xs) =
  if tn==nm then ps else getPosByName tn xs

getLayerByName :: ObName -> MapObject -> ObLayer 
getLayerByName _ [] = 0 
getLayerByName tn ((Ob _ nm ly _ _):xs) =
  if tn==nm then ly else getLayerByName tn xs

getNameByPos :: Pos -> MapObject -> ObName 
getNameByPos _ [] = T.empty 
getNameByPos pos ((Ob _ nm _ ps _):xs) =
  if pos==ps then nm else getNameByPos pos xs

getOprByPos :: Pos -> MapObject -> ObProperty
getOprByPos _ [] = No
getOprByPos pos ((Ob _ _ _ ps op):xs) =
  if pos==ps then op else getOprByPos pos xs  

getLayerByPos :: Pos -> MapObject -> ObLayer 
getLayerByPos _ [] = 0 
getLayerByPos pos ((Ob _ _ ly ps _):xs) =
  if pos==ps then ly else getLayerByPos pos xs  

updatePosByName :: ObName -> Pos -> MapObject -> MapObject
updatePosByName _ _ [] = []
updatePosByName tnm pos (ob@(Ob ch nm ly _ op):xs) =
  if tnm==nm then Ob ch nm ly pos op:xs
             else ob:updatePosByName tnm pos xs

