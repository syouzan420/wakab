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
    "mvd" -> moveDialog (head ags)
    "stm" -> setMap (head ags)
    _ -> return ()
  case en of
    "pls" -> setPlayer
    _ -> return ()

lookupFromSections :: T.Text -> StateG T.Text
lookupFromSections tx = do
  textSections <- use txs
  let tsKeyValues = map (\(TS ti tx) -> (ti,tx)) textSections
  return (fromMaybe T.empty (lookup tx tsKeyValues))  

setPlayer :: StateG ()
setPlayer = ipl .= True

setMap :: T.Text -> StateG ()
setMap i = do
  mapText <- lookupFromSections ("map" <> i)  
  obMapText <- lookupFromSections ("omap" <> i)
  obPropText <- lookupFromSections ("oprop" <> i)
  let mapData = convertMap mapText
  let obPropNums = map read ((words . T.unpack) obPropText)
  let mapObject = makeObjectMap obMapText obPropNums
  mpd .= mapData
  mpo .= mapObject

moveDialog :: T.Text -> StateG () 
moveDialog title = do
  newText <- lookupFromSections title
  unless (newText==T.empty) $ do
    txw .= newText
    txv %= (<> "\n \n")

