module State where

import Control.Monad.State (State, MonadState(get, put), gets)
import Types (Instruction(..), StackEnv, ID, Binding(..), Env, Label)

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

-- Debug

debug :: String -> Mam ()
debug str = addCode [Comment str]

debugEnv :: Mam ()
debugEnv = getEnv >>= debug . (++) "env: " . show

-- Env

getStackEnv :: Mam [Env]
getStackEnv = gets env

getEnv :: Mam Env
getEnv = gets (head . env)

lookupEnv :: ID -> Mam (Maybe Binding)
lookupEnv x = lookup x <$> getEnv

isReg :: Binding -> Bool
isReg (BRegister _) = True
isReg (BEnclosed _) = False

extendEnv :: (ID, Binding) -> Mam ()
extendEnv ("_", _) = return ()
extendEnv newBind@(x, _) = do
  env' <- popEnv
  case lookup x env' of
    Just _  -> pushEnv $ map (\b -> if x == fst b then newBind else b) env'
    Nothing -> pushEnv (newBind : env')

pushEnv :: [(ID, Binding)] -> Mam ()
pushEnv env' = do
  mam <- get
  put $ mam { env = env' : env mam }

popEnv :: Mam Env
popEnv = do
  mam <- get
  put $ mam { env = tail $ env mam }
  return $ head $ env mam

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
