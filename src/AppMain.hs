module AppMain where

import Brick.BChan (newBChan,writeBChan)
import Brick.Main (App(..),showFirstCursor,customMain)
import Brick.AttrMap (attrMap)
import Control.Monad (void,forever)
import Control.Concurrent (threadDelay,forkIO)
import qualified Graphics.Vty as V
import Definition (Game,Name,CustomEvent(Ticking))
import Initialize (newGame)
import UI (drawUI)
import Event (appEvent)
import Attr (makeColors)


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
    threadDelay 1000000

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  void $ customMain initialVty buildVty (Just chan) theApp newGame

  putStrLn "Hello"
