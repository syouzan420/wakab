{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Event(appEvent) where

import Brick.Main (halt,ViewportScroll,viewportScroll,vScrollToEnd)
import Brick.Types (BrickEvent(..),EventM)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl ((.=),use)
import Control.Monad (when,unless)
import qualified Graphics.Vty as V
import Graphics.Vty.Input.Events (Modifier(..))
import qualified Data.Text as T
import Converter (makeTateText,getInfoFromChar,mapSize,inpToDir)
import Action (movePlayer,hitAction)
import Code (exeCode)
import Definition

makeLenses ''Game
makeLenses ''Chra

dbScroll :: ViewportScroll Name
dbScroll = viewportScroll Debug

okEvent :: EventM Name Game ()
okEvent = do
      inputMode <- use pmd
      case inputMode of
        Txt -> do
          itx .= True 
          tsc .= 0
        Ply -> do
          mapData <- use mpd
          charas <- use chs
          mapEffect <- use mpt
          mpt .= hitAction "player" charas (mapSize mapData) mapEffect 

keyEvent :: Input -> [Modifier] -> EventM Name Game ()
keyEvent inp mdf = do
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
      debug <- use dbg
      mapData <- use mpd
      mapObject <- use mpo
      mapPos <- use mpp
      charas <- use chs
      evAct <- use eva
      let player = head charas
      let pDir = player^.dir
      let pPos = player^.pos
      let isDiag = MMeta `elem` mdf
      let keyDir = (\d -> if d==NoDir then pDir else d) $ inpToDir isDiag inp
      let isSameDir = pDir == keyDir
      let (nmpo,nmpp,nplp,evt) = if isSameDir
            then movePlayer inp isDiag mapWinSize mapPos mapData mapObject
            else (mapObject,mapPos,pPos,[]) 
      let nPlayer = player{_dir=keyDir,_pos=nplp}
      exeEvActs evt evAct
      chs .= nPlayer:tail charas
      mpo .= nmpo
      mpp .= nmpp
      evp .= evt
      dbg .= debug <> "\n" <> T.pack (show evt) <> "--" <> T.pack (show evAct) 

exeEvActs :: [PEvent] -> [EvAct] -> EventM Name Game () 
exeEvActs [] neas = eva .= neas 
exeEvActs (pe:pes) eas = do
  let (neas,ncds) = getCodes pe ([],[]) eas
  unless (null ncds) $ mapM_ exeCode ncds 
  exeEvActs pes neas

getCodes :: PEvent -> ([EvAct],[Code]) -> [EvAct] -> ([EvAct],[Code])
getCodes _ necs [] = necs
getCodes pe (neas,ncds) (ea@(EvAct te cd i):eas) =
  let ast = checkAct pe ea 
   in case ast of
        NAct -> getCodes pe (neas<>[ea],ncds) eas
        TAct -> getCodes pe (neas<>[EvAct te cd (i+1)],ncds) eas
        EAct -> getCodes pe (neas,ncds<>[cd]) eas

checkAct :: PEvent -> EvAct -> Ast 
checkAct pe (EvAct te _ co) =
  let evis = T.splitOn "." te 
   in if length evis==4 then  
        let (act:tp:cont:num:_) = evis
            isAct = case act of
              "ride" -> case pe of
                          PRide (Ob ch _ _ _ _) -> case tp of
                                                        "ch" -> cont==T.pack [ch] 
                                                        _ -> False
                          _ -> False
              _ -> False
         in if isAct then if co+1==(read . T.unpack) num then EAct else TAct 
                     else NAct 
                        else NAct 


textUpdate :: EventM Name Game ()
textUpdate = do
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
    when isCode $ do
      exeCode codeText
    newWholeText <- use txw 
    let isNewDialog = wholeText /= newWholeText 
    tct .= if isNewDialog then 0 else textCount + scanLength

effectUpdate :: EventM Name Game ()
effectUpdate = do
  mapEffect <- use mpt
  mpt .= scanEffect mapEffect

scanEffect :: MapObject -> MapObject
scanEffect [] = []
scanEffect (ob@(Ob ch nm ly ps pr):xs) 
  | ch=='/' = Ob '\\' nm ly ps pr:scanEffect xs 
  | ch=='\\' = scanEffect xs
  | otherwise = ob:scanEffect xs

appEvent :: BrickEvent Name CustomEvent -> EventM Name Game ()
appEvent e =
  case e of
    VtyEvent (V.EvKey V.KEsc []) -> halt
    VtyEvent (V.EvKey (V.KChar kch) mdf) -> do
      let input = case kch of 'l'->Ri; 'h'->Lf; 'k'->Up; 'j'->Dn; ' '->Ok; _->Dm
      case input of Ok -> okEvent; Dm -> return (); _ -> keyEvent input mdf
    AppEvent Ticking -> do
      textUpdate
      effectUpdate
      vScrollToEnd dbScroll
    _ -> return ()
