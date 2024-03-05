{-# LANGUAGE OverloadedStrings #-}
module Load(makeText) where

import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO)
import Data.Functor ((<&>))
import File (fileRead)
import Definition (textFile,TextSection,MapWhole)
import Converter (getText)

loadText :: MonadIO m => Int -> m T.Text
loadText i = fileRead (textFile++show i++".txt")

makeText :: MonadIO m => Int -> m [TextSection]
makeText i = loadText i <&> getText

