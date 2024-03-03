{-# LANGUAGE OverloadedStrings #-}
module Initialize(newGame) where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Definition (Game(..),Chra(..))

newGame :: Game
newGame = Game{_txd=[],_txs=[],_txw=T.empty,_txv=T.empty,_tct=0,_itx=True
              ,_mpd=[],_chs=[initChra]}

initChra :: Chra
initChra = Chra{_nme="player",_pos=V2 0 0,_hnd=(Nothing,Nothing)}

