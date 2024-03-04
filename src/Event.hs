{-# LANGUAGE TemplateHaskell #-}
module Event(appEvent) where

import Brick.Main (halt)
import Brick.Types (BrickEvent(..),EventM)
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl ((.=),use)
import Control.Monad (when,unless)
import qualified Graphics.Vty as V
import qualified Data.Text as T
import Converter (makeTateText,getInfoFromChar)
import Code (exeCode)
import Definition

makeLenses ''Game

rightKeyEvent :: EventM Name Game ()
rightKeyEvent = do
  inputMode <- use pmd
  isText <- use itx
  case inputMode of
    Txt -> unless isText $ do 
              textView <- use txv
              let columns = T.length$head$makeTateText textHeight textView
              scroll <- use tsc
              tsc .= if scroll < columns - textWidth then scroll + 1
                                                     else scroll
    Ply -> return ()

leftKeyEvent :: EventM Name Game ()
leftKeyEvent = do
  inputMode <- use pmd
  isText <- use itx
  case inputMode of
    Txt -> unless isText $ do 
              scroll <- use tsc
              tsc .= if scroll>0 then scroll - 1 else scroll
    Ply -> return ()

appEvent :: BrickEvent Name CustomEvent -> EventM Name Game ()
appEvent e =
  case e of
    VtyEvent (V.EvKey V.KEsc []) -> halt
    VtyEvent (V.EvKey (V.KChar 'l') []) -> rightKeyEvent
    VtyEvent (V.EvKey (V.KChar 'h') []) -> leftKeyEvent
    VtyEvent (V.EvKey (V.KChar ' ') []) -> do
      inputMode <- use pmd
      case inputMode of
        Txt -> do
          itx .= True 
          tsc .= 0
        Ply -> return ()
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
