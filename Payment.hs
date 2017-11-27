import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

pay :: Int -> TVar Int -> TVar Int -> STM ()
pay qty from to =
  do
    fromBal <- readTVar from
    toBal <- readTVar to
    writeTVar from (fromBal - qty)
    writeTVar to (toBal + qty)
