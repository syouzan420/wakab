{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Code(exeCode) where

import Brick.Types (EventM)
import Control.Monad (unless,when)
import qualified Data.Text as T
import Linear.V2 (V2(..))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl ((.=),(%=),use)
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import Converter (convertMap,makeObjectMap,setMapStartPos)
import Object (getPosByName)
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
    "a" -> when (length ags==2) $ setEventAction (head ags) (last ags) 
    "mvdi" -> moveDialog (head ags)
    "stmp" -> setMap (head ags)
    _ -> return ()
  case en of
    "stpl" -> setPlayer
    "txt" -> setText
    _ -> return ()

lookupFromSections :: T.Text -> StateG T.Text
lookupFromSections tx = do
  textSections <- use txs
  let tsKeyValues = map (\(TS ti t) -> (ti,t)) textSections
  return (fromMaybe T.empty (lookup tx tsKeyValues))  

setPlayer :: StateG ()
setPlayer = pmd .= Ply 

setText :: StateG ()
setText = pmd .= Txt

setEventAction :: T.Text -> T.Text -> StateG ()
setEventAction ev ac = eva %= (<> [EvAct ev (T.replace "." "_" ac) 0]) 
  

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
  let pps = getPosByName "player" mapObject
  let mapSize = V2 (fromIntegral$length$head mapData) (fromIntegral$length mapData)
  let mpos = setMapStartPos pps mapWinSize mapSize
  mpp .= mpos
  mpd .= mapData
  mpo .= mapObject 

moveDialog :: T.Text -> StateG () 
moveDialog title = do
  newText <- lookupFromSections title
  unless (newText==T.empty) $ do
    pmd .= Txt
    tct .= 0
    txw .= newText
    txv %= (<> "\n \n")

