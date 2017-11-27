import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

push :: a -> TVar [a] -> STM ()
push v stk =
  do
    currStk <- readTVar stk
    writeTVar stk (v:currStk)

-- This operation can fail if we attempt to pop an empty stack!
pop :: TVar [a] -> STM a
pop stk =
  do
    currStk <- readTVar stk
    case currStk of
      [] -> retry
      x:xs -> do
        writeTVar stk xs
        return x

-- This version tries popping only once
tryPop :: TVar [a] -> STM (Maybe a)
tryPop stk = orElse
             (do
                 hd <- pop stk
                 return (Just hd))
             (return Nothing)

main :: IO ()
main =
  do
    stmStack <- newTVarIO []
    done1 <- async $ atomically (push 12 stmStack)
    done2 <- async $ atomically (tryPop stmStack)
    done3 <- async $ atomically (push 15 stmStack)
    _ <- wait done1; top <- wait done2; _ <- wait done3
    currStack <- readTVarIO stmStack
    putStrLn ("Top of stack: " ++ show top)
    putStrLn ("Current stack contents: " ++ show currStack)
