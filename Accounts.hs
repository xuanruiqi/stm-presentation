createAccounts :: STM (TVar Int, TVar Int, TVar Int)
createAccounts =
  do
    mark <- newTVar 100
    alex <- newTVar 95
    ray  <- newTVar 84
    return (mark, alex, ray)
