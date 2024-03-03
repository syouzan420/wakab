{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Code(exeCode) where

import Brick.Types (EventM)
import Control.Monad (unless)
import qualified Data.Text as T
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl ((.=),use)
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import Definition

makeLenses ''Game

type StateG = EventM Name Game ()

exeCode :: T.Text -> StateG 
exeCode evt = do 
  let etxs = T.split (==' ') evt
  mapM_ exeOneCode etxs

exeOneCode :: T.Text -> StateG 
exeOneCode evt = do
  let en_ags = T.split (=='_') evt
  let (en,ags) = fromMaybe ("null",[]) (uncons en_ags)
  unless (null ags) $ case en of
    "mvd" -> moveDialog (head ags)
    _ -> return ()
  case en of
    "pls" -> setPlayer
    _ -> return ()

setPlayer :: StateG 
setPlayer = ipl .= True

moveDialog :: T.Text -> StateG 
moveDialog title = do
  textSections <- use txs
  let tsKeyValues = map (\(TS ti tx) -> (ti,tx)) textSections
  let newText = fromMaybe T.empty (lookup title tsKeyValues)
  unless (newText==T.empty) $ do
    txw .= newText
    tct .= 0

