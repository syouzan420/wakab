{-# LANGUAGE OverloadedStrings #-}
module Converter(makeTextDataT,getMapAndText) where

import Data.Maybe (fromMaybe)
import Linear.V2 (V2(..))
import Foreign.C.Types (CInt)
import qualified Data.Text as T
import Definition (mapCh,Pos,TextType(..),TextData(..),TextSection(..),MapWhole,MapCell(..))

type InitPos = Pos
type Indent = CInt
type HeightLimit = CInt 

makeTextDataT :: InitPos -> Indent -> HeightLimit 
                              -> TextType -> T.Text -> [TextData]
makeTextDataT pos ind hl ttp tx = 
  let (ch,txs) = fromMaybe ('0',T.empty) (T.uncons tx) 
   in case ch of
        '\\' -> let (code,rtxs) = getCodeData txs
                in Code code : makeTextDataT pos ind hl ttp rtxs 
        _ -> if txs==T.empty 
              then [Txt ttp pos ch] 
              else let (hd,tl) = fromMaybe ('0',T.empty) (T.uncons txs)
                    in if hd=='：' 
                        then 
           let (rb,tl2) = getRubi tl 
            in TxtR ttp pos ch rb : makeTextDataT (newPosT pos ind hl) ind hl ttp tl2
                        else
               Txt ttp pos ch : makeTextDataT (newPosT pos ind hl) ind hl ttp txs 

getCodeData :: T.Text -> (T.Text,T.Text)
getCodeData = T.break (=='\n')

getRubi :: T.Text -> (T.Text,T.Text)
getRubi tx = let (rb,tl) = T.break (=='：') tx
                 tl2
                    | tl==T.empty = T.empty 
                    | T.head tl == '：' = T.tail tl
                    | otherwise = tl
              in (rb,tl2)

newPosT :: Pos -> Indent -> HeightLimit -> Pos 
newPosT (V2 x y) ind hl = 
  let ty = y + 1
      isLimit = ty > hl
      nx = if isLimit then x - 3 else x 
      ny = if isLimit then ind else ty
   in V2 nx ny

getMapAndText :: T.Text -> ([TextSection],[MapWhole])
getMapAndText = sepMapAndText . getSections . T.lines

sepMapAndText :: [TextSection] -> ([TextSection],[MapWhole])
sepMapAndText = sepMapAndText' [] []

sepMapAndText' :: [TextSection] -> [MapWhole] -> [TextSection] -> ([TextSection],[MapWhole])
sepMapAndText' ts mw [] = (ts,mw) 
sepMapAndText' ts mw (x@(TS t dt):xs) =
  if T.length t > 3 && T.take 3 t == "map"
      then sepMapAndText' ts (mw++[convertMap dt]) xs 
      else sepMapAndText' (ts++[x]) mw xs

convertMap :: T.Text -> MapWhole
convertMap tx =
  let lns = T.lines tx
   in map (map (\ch -> toEnum (fromMaybe 0 (T.findIndex (==ch) mapCh)) :: MapCell) . T.unpack) lns


getSections :: [T.Text] -> [TextSection]
getSections = getSections' Nothing []

getSections' :: Maybe T.Text -> [T.Text] -> [T.Text] -> [TextSection]
getSections' _ [] [] = []
getSections' Nothing [] (x:xs) = if T.last x == ':' 
                    then getSections' (Just (T.init x)) [] xs 
                    else getSections' Nothing [] xs
getSections' Nothing _ _ = []
getSections' (Just tx) acc [] = [TS tx (T.unlines acc)]
getSections' (Just tx) acc (x:xs) = if T.last x == ':' 
                    then TS tx (T.unlines acc) : getSections' (Just (T.init x)) [] xs 
                    else getSections' (Just tx) (acc++[x]) xs  
                      



