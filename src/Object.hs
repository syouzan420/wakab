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

getObjByPos :: Pos -> ObLayer -> MapObject -> Object
getObjByPos _ _ [] = Ob ' ' T.empty 0 (V2 0 0) No
getObjByPos pos lay (ob@(Ob _ _ ly ps _):xs) = 
  if pos==ps && lay==ly then ob else getObjByPos pos lay xs 

getNameByPos :: Pos -> MapObject -> ObName 
getNameByPos _ [] = T.empty 
getNameByPos pos ((Ob _ nm _ ps _):xs) =
  if pos==ps then nm else getNameByPos pos xs

getOprByPos :: Pos -> ObLayer -> MapObject -> ObProperty
getOprByPos _ _ [] = No
getOprByPos pos lay ((Ob _ _ ly ps op):xs) =
  if pos==ps && lay==ly then op else getOprByPos pos lay xs  

getLayerByPos :: Pos -> MapObject -> ObLayer 
getLayerByPos _ [] = 0 
getLayerByPos pos ((Ob _ _ ly ps _):xs) =
  if pos==ps then ly else getLayerByPos pos xs  

updatePosByName :: ObName -> Pos -> MapObject -> MapObject
updatePosByName _ _ [] = []
updatePosByName tnm pos (ob@(Ob ch nm ly _ op):xs) =
  if tnm==nm then Ob ch nm ly pos op:xs
             else ob:updatePosByName tnm pos xs

