{-# LANGUAGE OverloadedStrings #-}
module Definition where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Foreign.C.Types (CInt)

type Pos = V2 CInt
type Title = T.Text
data TextSection = TS Title T.Text deriving (Eq,Show)
data TextType = Nml | Wst | Img deriving (Eq,Show)

-- Zenkaku, Hankaku, Maru(Stop), Ten(Wait), BackSlash(CodeStart)
data CharType = Zen | Han | Maru | Ten | Bks deriving (Eq,Show)

data TextData = Txt TextType Pos Char 
              | TxtR TextType Pos Char T.Text 
              | Code T.Text 
                      deriving (Eq,Show)

data MapCell = Path | Road | Field | Wood | Forest | Wall | Block | Water 
                                                          deriving (Eq,Show,Enum)
type MapWhole = [[MapCell]]

type ObName = T.Text
data Object = Ob ObName Pos ObProperty deriving (Eq,Show)
data ObProperty = Fr | Bl | Mv deriving (Eq,Show)
type MapObject = [Object]


data Shape = Round | Cubic | Flat deriving (Eq,Show)
data Element = A | I | U | E | O deriving (Eq,Show)
type Size = Int
type Hardness = Int
type Temperature = Int
type Power = Int
data Direction = East | EN | North | NW | West | WS | South | SE deriving (Eq,Show)
data Verb = Be | Hit | Throw | Emit | Guard | Use deriving (Eq,Show)
data Mana = Mana T Y deriving (Eq,Show)
data Arg = Arg Direction Mana deriving (Eq,Show)
data T = T{shape :: Shape,
           size :: Size,
           hardness :: Hardness,
           temperature :: Temperature,
           power :: Power,
           element :: Element
          } deriving (Eq,Show)
data Y = Y Verb [Arg] deriving (Eq,Show)

--nme: name , pos: position, hnd: hand (left,right)
data Chra = Chra{_nme :: T.Text, _pos :: Pos, _hnd :: (Maybe Mana,Maybe Mana)} 
                        deriving (Eq,Show)

--txd: text data, txs: text sections
--txw: tate text whole, txv: tate text view
--tct: text count
--itx: is text showing? 
--mpd: map datas, chs: characters(head is the player)
data Game = Game{_txd :: ![TextData], _txs :: ![TextSection]
                ,_txw :: !T.Text, _txv :: !T.Text
                ,_tct :: !Int
                ,_itx :: !Bool
                ,_ipl :: !Bool
                ,_mpd :: ![MapWhole], _chs :: ![Chra] }
                        deriving (Eq,Show)

data CustomEvent = Ticking deriving Show

data Name = View | Mess deriving (Eq,Ord,Show)

textFile :: FilePath
textFile = "text/waka"

mapCh :: T.Text
mapCh = "0123456789abcdefghi"

textInitPos :: Pos
textInitPos = V2 45 3 

textIndent :: CInt
textIndent = 3

textHeightLimit :: CInt
textHeightLimit = 22 
