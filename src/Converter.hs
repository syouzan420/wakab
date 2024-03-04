{-# LANGUAGE OverloadedStrings #-}
module Converter(getMapAndText,makeTateText,makeRectText,getInfoFromChar) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Definition (mapCh,TextSection(..),MapWhole,MapCell(..))

type Width = Int
type Height = Int
type Scroll = Int

--makeRectText :: Scroll -> Width -> Height -> T.Text -> [T.Text]
--makeRectText s w h tx = map (takeWidth s w . T.reverse) $ T.transpose $ concatMap  (takeHeight h) (T.lines (T.replace " " "　" tx))

makeRectText :: Scroll -> Width -> Height -> T.Text -> [T.Text]
makeRectText s w h tx = map (takeWidth s w) (makeTateText h tx)

makeTateText :: Height -> T.Text -> [T.Text]
makeTateText h tx = map T.reverse $ T.transpose $ concatMap (takeHeight h) (T.lines (T.replace " " "　" tx))

takeWidth :: Scroll -> Width -> T.Text -> T.Text
takeWidth s w tx = let lngT = T.length tx 
                    in if w > lngT then T.replicate (w-lngT) "　" <> tx 
                                   else T.take w (T.drop s tx)

takeHeight :: Height -> T.Text -> [T.Text]
takeHeight h tx
  | tx==T.empty = []
  | otherwise = let t = T.take h tx 
                    lngT = T.length t
                 in if lngT < h then [t <> T.replicate (h-lngT) "　"]   
                                else t : takeHeight h (T.drop h tx)

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
getSections' Nothing [] (x:xs) 
    | x==T.empty = getSections' Nothing [] xs
    | T.last x == ':' = getSections' (Just (T.init x)) [] xs 
    | otherwise = getSections' Nothing [] xs
getSections' Nothing _ _ = []
getSections' (Just tx) acc [] = [TS tx (T.unlines acc)]
getSections' (Just tx) acc (x:xs) 
    | x==T.empty = getSections' (Just tx) (acc++[" "]) xs
    | T.last x == ':' = TS tx (T.unlines acc) : getSections' (Just (T.init x)) [] xs 
    | otherwise = getSections' (Just tx) (acc++[x]) xs  
                      
type IsStop = Bool
type IsTyping = Bool
type IsCode = Bool

getInfoFromChar :: T.Text -> Int -> (IsStop,IsTyping,IsCode,Char,T.Text,Int)
getInfoFromChar wtx i = 
  let ch = T.index wtx i
      isCode = ch=='\\'
      isStop = ch=='。'
      isTyping = not (isStop || ch=='、' || isCode)
      codeText = if isCode then T.takeWhile (/='\n') (T.drop i wtx) else T.empty
      scanLength = if isCode then T.length codeText else 1
   in (isStop,isTyping,isCode,ch,codeText,scanLength)

