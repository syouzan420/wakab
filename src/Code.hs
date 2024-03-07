{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Code(exeCode) where

import Brick.Types (EventM)
import Control.Monad (unless)
import qualified Data.Text as T
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl ((.=),(%=),use)
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import Converter (convertMap,makeObjectMap)
import Definition

makeLenses ''Game

type StateG = EventM Name Game

exeCode :: T.Text -> StateG ()
exeCode evt = do 
  let etxs = T.split (==' ') evt
  mapM_ exeOneCode etxs

exeOneCode :: T.Text -> StateG () 
exeOneCode evt = do
  let en_ags = T.split (=='_') evt
  let (en,ags) = fromMaybe ("null",[]) (uncons en_ags)
  unless (null ags) $ case en of
    "mvdi" -> moveDialog (head ags)
    "stmp" -> setMap (head ags)
    _ -> return ()
  case en of
    "stpl" -> setPlayer
    _ -> return ()

lookupFromSections :: T.Text -> StateG T.Text
lookupFromSections tx = do
  textSections <- use txs
  let tsKeyValues = map (\(TS ti t) -> (ti,t)) textSections
  return (fromMaybe T.empty (lookup tx tsKeyValues))  

setPlayer :: StateG ()
setPlayer = pmd .= Ply 

setMap :: T.Text -> StateG ()
setMap i = do
  mapText <- lookupFromSections ("map" <> i)  
  obMapText <- lookupFromSections ("omap" <> i)
  obPropText <- lookupFromSections ("oprp" <> i)
  let mapData = convertMap mapText
  let obPropNLayerNums = map ((\(p,dl) -> (read p,read$tail dl)) . break (=='-'))
                                                    ((words . T.unpack) obPropText)
  let mapObjectPre = makeObjectMap obMapText obPropNLayerNums
  mapObject <- mapM (\(Ob ch nme l ps pr) -> lookupFromSections ("name" <> T.singleton ch) >>= (\nm -> return (Ob ch (if nm==T.empty then nme else T.init nm) l ps pr))) mapObjectPre
  mpd .= mapData
  mpo .= mapObject 

moveDialog :: T.Text -> StateG () 
moveDialog title = do
  newText <- lookupFromSections title
  unless (newText==T.empty) $ do
    txw .= newText
    txv %= (<> "\n \n")

