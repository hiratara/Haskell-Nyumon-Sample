{-# LANGUAGE OverloadedStrings #-}
module Supervisor (supervisor) where

import Data.Monoid ((<>))
import Data.Text as T
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception.Safe

-- Supervisor:
--  1. Supervisor launches Worker
--  2. Supervisor catches exceptions raising from Worker and re-launch Worker
--  3. Supervisor stops Worker when Supervisor catches async exceptions
--  4. Supervisor doesn't drop log messages left by Worker
--  5. Supervisor doesn't consume computation resources
supervisor :: TQueue T.Text -> IO () -> IO ()
supervisor queue action = do
    atomically $ writeTQueue queue $ "SUPERVISOR: launch worker"
    loop
  where
    loop = do
        result <- mask $ \restore -> do
            result <- bracket
                    (async $ restore action) -- NOTE: `action` runs with Unmasked despite `bracket` masks here
                    (\as -> cancel as)
                    (\as -> waitCatch as) -- NOTE: async-exceptions can interrupt in `waitCatch` because `loop` thread is blocked
            case result of
                Left e -> atomically $ writeTQueue queue $ "SUPERVISOR: catch exception: " <> T.pack (show e) -- NOTE: it cannot be interrupted due to Masked action
                Right _ -> return ()
            return result
        case result of
            Left _e -> do
                atomically $ writeTQueue queue $ "SUPERVISOR: re-launch worker"
                loop
            Right _ -> return ()

