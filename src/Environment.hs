module Environment(Id, Env, emptyEnv, lookupEnv, extendEnv) where

type Id = String

newtype Env a = EE [(Id, a)]

instance Show (Env a) where
  show (EE _) = "<entorno>"

emptyEnv :: Env a
emptyEnv = EE []

lookupEnv :: Env a -> Id -> a
lookupEnv (EE e) x =
  case lookup x e of
    Just y  -> y
    Nothing -> error ("La variable " ++ x ++ " no está definida.")

extendEnv :: Env a -> Id -> a -> Env a
extendEnv (EE e) x a = EE ((x, a) : e)

