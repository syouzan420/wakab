{-# LANGUAGE OverloadedStrings #-}
module Initialize where

import Linear.V2 (V2(..))
import Definition (Game(..),Chra(..))

newGame :: Game
newGame = Game{_txd=[],_txs=[],_itx=True,_its=True,_num=0,_mpd=[],_chs=[initChra]}

initChra :: Chra
initChra = Chra{nme="player",pos=V2 0 0,hnd=(Nothing,Nothing)}

