module State where

import Control.Monad.State (State, MonadState(get, put), gets)
import Types (Instruction, StackEnv, ID, Binding(..), Env, Reg)

type Mam = State MamState

data MamState = MamState {
  env      :: StackEnv,
  code     :: [Instruction],
  routines :: [Instruction],
  nextReg  :: Int,
  nextRtn  :: Int
}

initState :: MamState
initState = MamState {
  env      = [[]],
  code     = [],
  routines = [],
  nextReg  = 0,
  nextRtn  = 0
}

-- Env

getEnv :: Mam Env
getEnv = gets (head . env)

lookupEnv :: ID -> Mam (Maybe Binding)
lookupEnv x = lookup x <$> getEnv

lookupEnvRegister :: ID -> Mam Reg
lookupEnvRegister x = do
  res <- lookupEnv x
  case res of
    Just (BRegister reg) -> return reg
    _ -> do
      mam <- get
      env' <- getEnv
      error $ "'"++ x ++"' is not defined in "
        ++ show env'
        ++ "\nStackEnv: "
        ++ show (env mam)

extendEnv :: (ID, Binding) -> Mam ()
extendEnv bind = do
  env' <- popEnv
  let env'' = bind : env'
  pushEnv env''

pushEnv :: [(ID, Binding)] -> Mam ()
pushEnv env' = do
  mam <- get
  put $ mam { env = env' : env mam }

popEnv :: Mam Env
popEnv = do
  mam <- get
  put $ mam { env = tail $ env mam }
  return $ last $ env mam

-- Code

extendCode :: [Instruction] -> Mam ()
extendCode _code = do
  mam <- get
  put $ mam { code = code mam ++ _code }
