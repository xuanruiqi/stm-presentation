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
