module Event where

import Brick.Main (halt)
import Brick.Types (BrickEvent(..),EventM)
import qualified Graphics.Vty as V
import Definition

appEvent :: BrickEvent Name CustomEvent -> EventM Name Game ()
appEvent e =
  case e of
    VtyEvent (V.EvKey V.KEsc []) -> halt
    AppEvent Ticking -> do
      return ()
    _ -> return ()
