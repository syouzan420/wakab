{-# LANGUAGE TemplateHaskell #-}
module UI where

import Brick.Types (Widget(..))
import Brick.Widgets.Core (str)
import Brick.Widgets.Center as C
import Lens.Micro.TH (makeLenses)
import Definition (Game(..),Name)

makeLenses ''Game

drawUI :: Game -> [Widget Name]
drawUI game = [ui]
  where ui = C.center $ str "Hello World"
