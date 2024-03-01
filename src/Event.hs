{-# LANGUAGE TemplateHaskell #-}
module Event where

import Brick.Main (halt)
import Brick.Types (BrickEvent(..),EventM)
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl ((.=),use)
import qualified Graphics.Vty as V
import qualified Data.Text as T
import Definition

makeLenses ''Game

appEvent :: BrickEvent Name CustomEvent -> EventM Name Game ()
appEvent e =
  case e of
    VtyEvent (V.EvKey V.KEsc []) -> halt
    AppEvent Ticking -> do
      textCount <- use tct
      wholeText <- use txw
      txv .= T.take textCount wholeText 
      tct .= (textCount + 1)
      return ()
    _ -> return ()
