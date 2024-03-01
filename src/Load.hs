{-# LANGUAGE OverloadedStrings #-}
module Load(makeMapAndText) where

import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO)
import Data.Functor ((<&>))
import File (fileRead)
import Definition (textFile,TextSection,MapWhole)
import Converter (getMapAndText)

loadText :: MonadIO m => Int -> m T.Text
loadText i = fileRead (textFile++show i++".txt")

makeMapAndText :: MonadIO m => Int -> m ([TextSection],[MapWhole])
makeMapAndText i = loadText i <&> getMapAndText

