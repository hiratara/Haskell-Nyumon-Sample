module Logger where

import Data.Text as T
import Control.Concurrent.STM
import Control.Monad

logWriter :: TQueue T.Text -> (T.Text -> IO ()) -> IO ()
logWriter queue logfunc = forever $ do
    msg <- atomically $ readTQueue queue
    logfunc msg

