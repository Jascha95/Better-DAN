import Control.Monad.Trans.State.Lazy

-- ^ Ex 1
    
type Random a = State Int a
fresh :: Random Int
runPRNG :: Random a -> Int -> a

fresh = do
  v <- get
  let new_v = (6364136223846793005 * v + 1442695040888963407) `mod` 2^64
  put new_v
  return v

freshBool :: Random Bool
freshBool = fmap even fresh

runPRNG m s = fst $ runState m s
