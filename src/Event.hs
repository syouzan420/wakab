{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Event(appEvent) where

import Brick.Main (halt,ViewportScroll,viewportScroll,vScrollToEnd)
import Brick.Types (BrickEvent(..),EventM)
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl ((.=),use)
import Control.Monad (when,unless)
import qualified Graphics.Vty as V
import qualified Data.Text as T
import Converter (makeTateText,getInfoFromChar)
import Action (movePlayer)
import Code (exeCode)
import Definition

makeLenses ''Game

dbScroll :: ViewportScroll Name
dbScroll = viewportScroll Debug

keyEvent :: Input -> EventM Name Game ()
keyEvent inp = do
  inputMode <- use pmd
  case inputMode of
    Txt -> when (inp==Ri || inp==Lf) $ do 
      isText <- use itx
      unless isText $ do 
        scroll <- use tsc
        case inp of
          Ri -> do
            textView <- use txv
            let columns = T.length$head$makeTateText textHeight textView
            tsc .= if scroll < columns - textWidth then scroll + 1 else scroll
          Lf -> do
            tsc .= if scroll>0 then scroll - 1 else scroll
          _ -> return ()
    Ply -> do 
      mapData <- use mpd
      mapObject <- use mpo
      let nmpo = movePlayer inp mapData mapObject
      mpo .= nmpo


appEvent :: BrickEvent Name CustomEvent -> EventM Name Game ()
appEvent e =
  case e of
    VtyEvent (V.EvKey V.KEsc []) -> halt
    VtyEvent (V.EvKey (V.KChar 'l') []) -> keyEvent Ri
    VtyEvent (V.EvKey (V.KChar 'h') []) -> keyEvent Lf
    VtyEvent (V.EvKey (V.KChar 'k') []) -> keyEvent Up 
    VtyEvent (V.EvKey (V.KChar 'j') []) -> keyEvent Dn 
    VtyEvent (V.EvKey (V.KChar ' ') []) -> do
      inputMode <- use pmd
      case inputMode of
        Txt -> do
          itx .= True 
          tsc .= 0
        Ply -> return ()
    AppEvent Ticking -> do
      debug <- use dbg
      isText <- use itx
      wholeText <- use txw
      textCount <- use tct
      inputMode <- use pmd
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
        when isCode $ do
          exeCode codeText
        newWholeText <- use txw 
        let isNewDialog = wholeText /= newWholeText 
        tct .= if isNewDialog then 0 else textCount + scanLength
  --    dbg .= debug <> "\n" <> T.pack (show inputMode)
      vScrollToEnd dbScroll
    _ -> return ()
