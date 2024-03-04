module NotUse where

import qualified Data.Text as T
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import Data.Maybe (fromMaybe)
import Definition (Pos,TextType(..),TextData(..))

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
              then [Tx ttp pos ch] 
              else let (hd,tl) = fromMaybe ('0',T.empty) (T.uncons txs)
                    in if hd=='：' 
                        then 
           let (rb,tl2) = getRubi tl 
            in TxR ttp pos ch rb : makeTextDataT (newPosT pos ind hl) ind hl ttp tl2
                        else
               Tx ttp pos ch : makeTextDataT (newPosT pos ind hl) ind hl ttp txs 

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

elimCode :: T.Text -> T.Text
elimCode = elimCode' False

elimCode' :: Bool -> T.Text -> T.Text
elimCode' False tx =
  let (ch,txs) = fromMaybe ('0',T.empty) (T.uncons tx)
   in if txs == T.empty then T.singleton ch
                        else if ch=='\\' then elimCode' True txs 
                                         else T.singleton ch <> elimCode' False txs
elimCode' True tx =
  let (ch,txs) = fromMaybe ('0',T.empty) (T.uncons tx)
   in if txs == T.empty then T.empty
                       else case ch of
                         '\n' -> T.singleton ch <> elimCode' False txs 
                         '\\' -> elimCode' False txs
                         _ -> elimCode' True txs

