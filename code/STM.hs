import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

-- Adapted from Real World Haskell, Ch. 28 (Software Transactional Memory)
pay :: Int -> TVar Int -> TVar Int -> STM ()
pay qty from to =
  do
    fromBal <- readTVar from
    toBal <- readTVar to
    writeTVar from (fromBal - qty)
    writeTVar to (toBal + qty)

createAccounts :: STM (TVar Int, TVar Int, TVar Int)
createAccounts =
  do
    mark <- newTVar 100
    alex <- newTVar 95
    ray  <- newTVar 84
    return (mark, alex, ray)

main :: IO ()
main =
  do
    (mark, alex, ray) <- atomically createAccounts
    done1 <- async $ atomically (pay 10 mark ray)
    done2 <- async $ atomically (pay 10 mark alex)
    done3 <- async $ atomically (pay 5 ray alex)
    mapM_ wait [done1, done2, done3]
    markBal <- readTVarIO mark
    rayBal <- readTVarIO ray
    alexBal <- readTVarIO alex
    putStrLn ("Mark has: $" ++ show markBal)
    putStrLn ("Alex has: $" ++ show alexBal)
    putStrLn ("Ray has: $"  ++ show rayBal)
