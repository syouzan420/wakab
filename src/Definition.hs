{-# LANGUAGE OverloadedStrings #-}
module Definition where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Foreign.C.Types (CInt)

type Pos = V2 CInt
type Title = T.Text
data TextSection = TS Title T.Text deriving (Eq,Show)

-- map
data MapCell = Field | Path | Road | Wood | Forest | Wall | Block | Water 
                                                          deriving (Eq,Show,Enum)
type MapWhole = [[MapCell]]

type ObChar = Char
type ObName = T.Text
type ObLayer = Int
data Object = Ob ObChar ObName ObLayer Pos ObProperty deriving (Eq,Show)
data ObProperty = Pl | Ch | En | Ef | Fr | Bl | Mv | No deriving (Eq,Show,Enum)
--Player,Chara,Enemy,Effect,Free,Block,Move,Nothing
type MapObject = [Object]


-- mana
data Shape = Round | Cubic | Flat | Pole | NoShape deriving (Eq,Show)
data Element = A | I | U | E | O deriving (Eq,Show)
type Size = Int
type Hardness = Int
type Temperature = Int
type Power = Int
data Direction = East | EN | North | NW | West | WS | South | SE | NoDir
                                                           deriving (Eq,Show)
data Verb = Be | Hit | Throw | Emit | Guard | Use deriving (Eq,Show)
data Mana = Mana T Y deriving (Eq,Show)
data Arg = Arg Direction Mana deriving (Eq,Show)
data T = T{_shape :: Shape,
           _size :: Size,
           _hardness :: Hardness,
           _power :: Power,
           _element :: Element
          } deriving (Eq,Show)
data Y = Y Verb [Arg] deriving (Eq,Show)

--nme: name , pos: position, hnd: hand (left,right)
data Chra = Chra{_nme :: T.Text, _pos :: Pos, _dir :: Direction
                ,_hnd :: (Maybe Mana,Maybe Mana)} 
                        deriving (Eq,Show)

--pmd: input mode (Txt, Ply)
--txs: text sections
--txw: tate text whole, txv: tate text view
--tct: text count, tsc: text scroll (from end)
--itx: is text showing? 
--mpd: map data, mpo: map objects, mpt: map objects changing with time
--chs: characters(head is the player)
--dbg: for debug
data Game = Game{_pmd :: !IMode
                ,_txs :: ![TextSection]
                ,_txw :: !T.Text, _txv :: !T.Text
                ,_tct :: !Int, _tsc :: !Int
                ,_itx :: !Bool
                ,_mpd :: !MapWhole, _mpo :: !MapObject, _mpt :: !MapObject
                ,_mpp :: !Pos
                ,_chs :: ![Chra]
                ,_dbg :: !T.Text
                } deriving (Eq,Show)

-- input mode : text mode, player mode
data IMode = Txt | Ply deriving (Eq,Show)

-- Ok, Cancel, Right, Up, Left, Down, Dummy
data Input = Ok | Cn | Ri | Up | Lf | Dn | Dm deriving (Eq,Show)

data CustomEvent = Ticking deriving Show

data Name = Mess | Map | Debug deriving (Eq,Ord,Show)

textFile :: FilePath
textFile = "text/waka"

mapCh :: T.Text
mapCh = "0123456789abcdefghi"

textWidth :: Int
textWidth = 15

textHeight :: Int
textHeight = 18

mapWinSize :: Pos
mapWinSize = V2 12 6 

