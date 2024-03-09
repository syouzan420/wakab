{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module UI(drawUI) where

import Brick.Types (Widget(..),ViewportType(..))
import Brick.Widgets.Core (txt,txtWrap,(<+>),(<=>),hLimit,vLimit,viewport)
--import Brick.Widgets.Center as C
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import qualified Data.Text as T
import Converter (makeRectText,showMap,putMapInFrame)
import Definition (Game(..),Name(..),textWidth,textHeight,mapWinSize)

makeLenses ''Game

drawUI :: Game -> [Widget Name]
drawUI game = [ui]
  where m = txt $ T.unlines 
                $ makeRectText (game^.tsc) textWidth textHeight (game^.txv)
        d = txtWrap $ game^.dbg
        p = txt $ putMapInFrame mapWinSize (game^.mpp) $ showMap (game^.mpo) (game^.mpd)
        ms = viewport Mess Vertical m
        mp = viewport Map Vertical p
        db = viewport Debug Vertical d
        ui = txt " " 
             <=> 
             (txt "  " <+> 
                    hLimit 40 (vLimit 20 ms) <+>
                    hLimit 20 (vLimit 15 mp))
             <=>
             hLimit 40 (vLimit 10 db)
                           

