{-# LANGUAGE TemplateHaskell #-}
module Event(appEvent) where

import Brick.Main (halt)
import Brick.Types (BrickEvent(..),EventM)
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl ((.=),use)
import Control.Monad (when)
import qualified Graphics.Vty as V
import qualified Data.Text as T
import Converter (getInfoFromChar)
import Code (exeCode)
import Definition

makeLenses ''Game

appEvent :: BrickEvent Name CustomEvent -> EventM Name Game ()
appEvent e =
  case e of
    VtyEvent (V.EvKey V.KEsc []) -> halt
    VtyEvent (V.EvKey (V.KChar ' ') []) -> do
      itx .= True 
    AppEvent Ticking -> do
      isText <- use itx
      wholeText <- use txw
      textCount <- use tct
      let textLength = T.length wholeText
      let isTextShowing = textCount < textLength
      when (isText && isTextShowing) $ do
        textView <- use txv
        let (isStop,isTyping,isCode,targetChar,codeText,scanLength) 
                              = getInfoFromChar wholeText textCount
        when isStop $ itx .= False
        when isTyping $ do
          let newTextView = textView <> T.singleton targetChar
          txv .= newTextView 
        when isCode $ exeCode codeText
        tct .= (textCount + scanLength)
    _ -> return ()
