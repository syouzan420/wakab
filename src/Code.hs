module Code(exeCode) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T

exeCode :: MonadIO m => T.Text -> m ()
exeCode code = undefined
