module AppMain where

import Brick.BChan (newBChan,writeBChan)
import Brick.Main (App(..),showFirstCursor,customMain)
import Brick.AttrMap (attrMap)
import Control.Monad (void,forever)
import Control.Concurrent (threadDelay,forkIO)
import qualified Graphics.Vty as V
import Definition (Game(..),Name,CustomEvent(Ticking),TextSection(..))
import Initialize (newGame)
import UI (drawUI)
import Event (appEvent)
import Attr (makeColors)
import Load (makeMapAndText)


theApp :: App Game CustomEvent Name
theApp =
  App { appDraw = drawUI
      , appChooseCursor = showFirstCursor
      , appHandleEvent = appEvent
      , appStartEvent = return ()
      , appAttrMap = const $ attrMap V.defAttr (makeColors 20)
      }

appMain :: IO ()
appMain = do
  chan <- newBChan 1

  void $ forkIO $ forever $ do
    writeBChan chan Ticking
    threadDelay 100000

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  (sections,maps) <- makeMapAndText 0
  let (TS _ tx) = head sections
  let initGame = newGame{_txs=sections,_txw=tx,_mpd=maps}

  void $ customMain initialVty buildVty (Just chan) theApp initGame

  putStrLn "Hello"
