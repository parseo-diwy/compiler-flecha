module State where

import Control.Monad.State (State, MonadState(get, put), gets)
import Types (Instruction (Comment), StackEnv, ID, Binding(..), Env, Reg(Local), Label)
import Data.List (intercalate)

type Mam = State MamState

data Stack = CodeStack | RoutineStack Label

type CodeRoutine = (Label, [Instruction])

data MamState = MamState {
  env      :: StackEnv,
  code     :: [Instruction],
  routines :: [CodeRoutine],
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
debug str = addCode [Comment str]


-- Env

getStackEnv :: Mam [Env]
getStackEnv = gets env

getEnv :: Mam Env
getEnv = gets (head . env)

lookupEnv :: ID -> Mam (Maybe Binding)
lookupEnv x = do
  env' <- getEnv
  let res = lookup x env'
  return res

lookupEnvRegister :: ID -> Mam Reg
lookupEnvRegister x = do
  env' <- getEnv
  let regEnv = filter (isReg . snd) env'
  let res = lookup x regEnv
  -- -- res <- lookupEnv x
  case res of
    Just (BRegister reg) -> return reg
    _ -> do
      _ <- lookupEnvError x
      return $ Local "_err"

isReg :: Binding -> Bool
isReg (BRegister _) = True
isReg (BEnclosed _) = False

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
  return $ head $ env mam

lookupEnvError :: ID -> Mam String
lookupEnvError x = do
  env' <- getEnv
  stackEnv' <- getStackEnv
  let stackEnv'' = map show stackEnv'
  error $ "'" ++ show x ++ "' is not defined in "
    ++ show env'
    ++ "\nStackEnv: \n"
    ++ intercalate "\n" stackEnv''

-- Code Stack

getStack :: Mam Stack
getStack = gets currentStack

switchStack :: Stack -> Mam ()
switchStack stack = do
  mam <- get
  put $ mam { currentStack = stack }

switchToRoutineStack :: Label -> Mam ()
switchToRoutineStack label = switchStack $ RoutineStack label

isRoutineStack :: Mam Bool
isRoutineStack = do
  stack <- getStack
  case stack of
    CodeStack -> return False
    RoutineStack _ -> return True

-- Code

addCode :: [Instruction] -> Mam ()
addCode _code = do
  mam <- get
  case currentStack mam of
    CodeStack -> put $ mam { code = code mam ++ _code }
    RoutineStack label -> addCodeRoutine label _code

addCodeRoutine :: Label -> [Instruction] -> Mam ()
addCodeRoutine label _code = do
  mam <- get
  let stack = lookup label $ routines mam
  case stack of
    Just current -> do
      let rout = filter (\a -> fst a /= label) $ routines mam
      put $ mam { routines = rout ++ [(label, current ++ _code)] }
    Nothing -> do
      put $ mam { routines = routines mam ++ [(label, _code)] }
