import Control.Monad (forM_)
import Control.Exception (bracket_)
import Control.Concurrent

data SharedResource a = SharedResource QSem a

newSharedResource :: Int -> a -> IO (SharedResource a)
newSharedResource n v = do
  sem <- newQSem n
  return $ SharedResource sem v

withSharedResource :: SharedResource a -> (a -> IO ()) -> IO ()
withSharedResource (SharedResource sem v) action =
  bracket_
    (waitQSem sem)
    (signalQSem sem)
    (action v)

main :: IO ()
main = do
  sharedResource <- newSharedResource 3 ()

  forM_ [1..10 :: Int] $ \_i -> forkIO $ do
    withSharedResource sharedResource $ \_res -> do
      tid <- myThreadId
      forM_ [1..3 :: Int] $ \n -> do
        putStrLn $ show tid ++ ": " ++ show n
        threadDelay 500000

  threadDelay $ 10 * 1000000
