{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module UI(drawUI) where

import Brick.Types (Widget(..),ViewportType(..))
import Brick.Widgets.Core (txt,(<+>),(<=>),hLimit,vLimit,viewport)
--import Brick.Widgets.Center as C
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import qualified Data.Text as T
import Converter (makeRectText)
import Definition (Game(..),Name(..),textWidth,textHeight)

makeLenses ''Game

drawUI :: Game -> [Widget Name]
drawUI game = [ui]
  where m = txt $ T.unlines 
                $ makeRectText (game^.tsc) textWidth textHeight (game^.txv)
        ms = viewport Mess Vertical m
        ui = txt " " 
             <=> 
             (txt "  " <+> (hLimit 40 (vLimit 20 ms)
                           <=>
                           txt "Hello World"
                           )
             )
