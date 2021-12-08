module State where

import Control.Monad.State (State, MonadState(get, put), gets)
import Types (Instruction (Comment), StackEnv, ID, Binding(..), Env, Reg)
import Data.List (intercalate)

type Mam = State MamState

data Stack = CodeStack | RoutineStack

data MamState = MamState {
  env      :: StackEnv,
  code     :: [Instruction],
  routines :: [Instruction],
  nextReg  :: Int,
  nextRtn  :: Int,
  currentStack :: Stack
}

initState :: MamState
initState = MamState {
  env      = [[]],
  code     = [],
  routines = [],
  nextReg  = 0,
  nextRtn  = 0,
  currentStack = CodeStack
}

debug :: String -> Mam ()
debug str = addCode [Comment $ " " ++ str]


-- Env

getStackEnv :: Mam [Env]
getStackEnv = gets env

getEnv :: Mam Env
getEnv = gets (head . env)

lookupEnv :: ID -> Mam (Maybe Binding)
lookupEnv x = lookup x <$> getEnv

lookupEnvRegister :: ID -> Mam Reg
lookupEnvRegister x = do
  res <- lookupEnv x
  case res of
    Just (BRegister reg) -> return reg
    _ -> lookupEnvError x

lookupEnvBinding :: ID -> Mam (Maybe Int)
lookupEnvBinding x = do
  res <- lookupEnv x
  case res of
    Just (BEnclosed n) -> return $ Just n
    _ -> return Nothing

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

lookupEnvError :: Show a => [Char] -> Mam a
lookupEnvError x = do
  env' <- getEnv
  stackEnv' <- getStackEnv
  let stackEnv'' = map show stackEnv'
  error $ "'"++ show x ++"' is not defined in "
    ++ show env'
    ++ "\nStackEnv: \n"
    ++ intercalate "\n" stackEnv''

-- Code

getStack :: Mam Stack
getStack = gets currentStack

switchStack :: Stack -> Mam ()
switchStack stack = do
  mam <- get
  put $ mam { currentStack = stack }

switchToRoutineStack :: Mam ()
switchToRoutineStack = switchStack RoutineStack

addCode :: [Instruction] -> Mam ()
addCode _code = do
  mam <- get
  case currentStack mam of
    CodeStack -> put $ mam { code = code mam ++ _code }
    RoutineStack -> put $ mam { routines = routines mam ++ _code }
