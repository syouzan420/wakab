module Attr(atpl,athi,atms,makeColors) where

import Brick.AttrMap (attrName,AttrName)
import Brick.Util (fg)
import qualified Graphics.Vty as V

--player, hiding, message
atpl, athi, atms :: AttrName
atpl = attrName "player"
athi = attrName "0"
atms = attrName "message"

makeColors :: Int -> [(AttrName, V.Attr)]
makeColors 0 = [(attrName "player", fg V.brightCyan)
               ,(attrName "0", fg V.black)
               ,(attrName "message", fg V.brightYellow)
               ]
makeColors i = (attrName (show i), fg (V.rgbColor (i*10) (i*10) (i*10))):makeColors (i-1)
