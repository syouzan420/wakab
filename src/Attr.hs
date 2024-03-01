module Attr(atpl,athi,atms,makeColors) where

import Brick.AttrMap (attrName,AttrName)
import Brick.Util (fg)
import qualified Graphics.Vty as V

--player, hiding, message
atpl, atms, athi :: AttrName
atpl = attrName "player"
atms = attrName "message"
athi = attrName "0"

makeColors :: Int -> [(AttrName, V.Attr)]
makeColors 0 = [(attrName "player", fg V.brightCyan)
               ,(attrName "message", fg V.brightYellow)
               ,(attrName "0", fg V.black)
               ]
makeColors i = (attrName (show i), fg (V.rgbColor (i*10) (i*10) (i*10))):makeColors (i-1)
