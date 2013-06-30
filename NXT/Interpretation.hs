{-# LANGUAGE GADTs, DeriveDataTypeable, DoAndIfThenElse #-}
-- |
-- Module      : NXT.Interpretation
-- Copyright   : Alexander Thiemann <mail@agrafix.net>
-- License     : BSD3
--
-- Maintainer  : Alexander Thiemann <mail@agrafix.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module NXT.Interpretation
    ( runVM
    , RealEnv (..)
    , MotorState(..)
    )
where

import NXT.Types

import Data.IORef
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import qualified Data.HashMap.Strict as HM

type EnvChan = TBQueue RealEnv
data RealEnv
   = RealEnv
   { re_light :: Int
   , re_ultra :: Int
   } deriving (Show)

data MotorState
   = MotorState
   { m_a :: TVar Int
   , m_b :: TVar Int
   , m_c :: TVar Int
   }

getMS 0 = m_a
getMS 1 = m_b
getMS 2 = m_c

data SensorState
   = SensorState
   { s_1 :: TVar Int
   , s_2 :: TVar Int
   , s_3 :: TVar Int
   , s_4 :: TVar Int
   }

data SensorReads
   = SensorReads
   { sr_1 :: TVar (RealEnv -> Int)
   , sr_2 :: TVar (RealEnv -> Int)
   , sr_3 :: TVar (RealEnv -> Int)
   , sr_4 :: TVar (RealEnv -> Int)
   }

getSS :: Int -> (SensorState -> TVar Int)
getSS 0 = s_1
getSS 1 = s_2
getSS 2 = s_3
getSS 3 = s_4

getSR :: Int -> (SensorReads -> TVar (RealEnv -> Int))
getSR 0 = sr_1
getSR 1 = sr_2
getSR 2 = sr_3
getSR 3 = sr_4

data FunMap
   = FunMap
    { fm_funs :: HM.HashMap String FunDefinition
    , fm_apiCall  :: String -> [NXTVal] -> IO NXTVal
    }

type Env = IORef (HM.HashMap String (IORef NXTVal))

data NXTVal
   = NInt Int
   | NFloat Float
   | NStr String
   | NBool Bool
   | NVoid
   deriving (Eq, Show, Ord)

emptyEnv :: IO Env
emptyEnv = newIORef HM.empty

isBound :: Env -> String -> IO Bool
isBound envR var =
    readIORef envR >>= return . maybe False (const True) . HM.lookup var

getVar :: Env -> String -> IO NXTVal
getVar envR var =
    do env <- liftIO $ readIORef envR
       maybe (error $ "Getting an unbound variable " ++ var)
                 (liftIO . readIORef)
                 (HM.lookup var env)

setVar :: Env -> String -> NXTVal -> IO NXTVal
setVar envR var value =
    do env <- liftIO $ readIORef envR
       maybe (error $ "Setting an unbound variable " ++ var)
                 (liftIO . (flip writeIORef value))
                 (HM.lookup var env)
       return value

defineVar :: Env -> String -> NXTVal -> IO NXTVal
defineVar envR var value = do
    alreadyDefined <- liftIO $ isBound envR var
    if alreadyDefined
       then setVar envR var value >> return value
       else liftIO $ do
          valueR <- newIORef value
          env <- readIORef envR
          writeIORef envR (HM.insert var valueR env)
          return value

runVM :: MonadResource m => [FunDefinition] -> m (EnvChan, MotorState)
runVM fd =
    do env <- liftIO emptyEnv
       envChan <- liftIO $ atomically $ newTBQueue 20
       b <- liftIO motorState
       c <- liftIO sensorState
       d <- liftIO sensorReads

       case HM.lookup "main" funMap of
         Just fd ->
             do _ <- allocate (forkIO (updateLoop envChan c d)) (killThread)
                _ <- allocate (forkIO $ runDef (FunMap funMap (apiCall envChan b c d)) env fd) (killThread)
                return (envChan, b)
         Nothing ->
             error "No main task found."
    where
      updateLoop envQ sensor sensorR =
          do logE <- atomically $
                     do env <- readTBQueue envQ
                        mapM_ (\sensorId ->
                               do extractFun <- readTVar ((getSR sensorId) sensorR)
                                  let envVal = (extractFun env)
                                  writeTVar ((getSS sensorId) sensor) envVal
                              ) [0..3]
                        return env
             putStrLn $ "New Environment: " ++ (show logE)
             updateLoop envQ sensor sensorR

      funMap = foldl (\hm f@(FunDefinition name _ _ _) -> HM.insert name f hm) HM.empty fd
      apiCall env motor sensor sensorR funName input =
          do ret <- apiCall' env motor sensor sensorR funName input
             putStrLn $ "Called " ++ (show (funName, input))
             putStrLn $ "Got: " ++ show ret
             return ret
      apiCall' env motor sensor sensorR funName input =
          case (funName, input) of
            ("Wait", [NInt time]) ->
                do threadDelay (1000 * time)
                   return NVoid
            ("SetSensorLight", [NInt sensorId]) ->
                do atomically $ writeTVar ((getSR sensorId) sensorR) re_light
                   return NVoid
            ("Sensor", [NInt sensorId]) ->
                do ct <- atomically $ readTVar ((getSS sensorId) sensor)
                   return $ NInt ct
            ("OnFwd", [NInt motorId, NInt motorPwr]) ->
                do atomically $ writeTVar ((getMS motorId) motor) motorPwr
                   return NVoid
            x ->
                error $ "Call to undefined function " ++ (show x)

      motorState = MotorState <$> zero <*> zero <*> zero
      sensorState = SensorState <$> zero <*> zero <*> zero <*> zero
      sensorReads = SensorReads
                    <$> unknown
                    <*> unknown
                    <*> unknown
                    <*> unknown
      zero = atomically $ newTVar 0
      unknown = atomically $ newTVar $ (\_ -> -1)

runDef :: FunMap -> Env -> FunDefinition -> IO ()
runDef funMap env (FunDefinition _ _ _ body) =
    mapM_ (runStmt funMap env) body

runStmt :: FunMap -> Env -> Stmt -> IO ()
runStmt funMap env (If condR t f) =
    do (NBool cond) <- runV funMap env condR
       if cond
       then mapM_ (runStmt funMap env) t
       else mapM_ (runStmt funMap env) f
runStmt funMap env l@(While condR loop) =
    do (NBool cond) <- runV funMap env condR
       if cond
       then do mapM_ (runStmt funMap env) loop
               runStmt funMap env l
       else return ()
runStmt funMap env (DeclVar (V (VarP p))) =
    do defineVar env p NVoid
       return ()
runStmt funMap env (AssignVar (V (VarP p)) valR) =
    do val <- runV funMap env valR
       setVar env p val
       return ()

runStmt funMap env (Eval v) =
    do _ <- runV funMap env v
       return ()
runStmt funMap env (FunReturn v) =
    do val <- runV funMap env v
       defineVar env "__funReturnVal" val
       return ()

runV :: FunMap -> Env -> V a -> IO NXTVal
runV funMap env ct = runT funMap env (unpack ct)

runT :: FunMap -> Env -> T a -> IO NXTVal
runT _ _ Void = return NVoid
runT _ env (VarP p) = getVar env p
runT _ env (Lit i) = return $ NInt i
runT _ env (Rat i) = return $ NFloat i
runT _ env (StrLit str) = return $ NStr str
runT _ env (BoolLit b) = return $ NBool b

runT funMap env (CastOp "cast2int" val) =
     do evaluated <- runV funMap env val
        case evaluated of
          NFloat f -> return $ NInt $ floor f
          _ -> error "Invalid cast."
runT funMap env (CastOp "cast2float" val) =
     do evaluated <- runV funMap env val
        case evaluated of
          NInt i -> return $ NFloat $ fromIntegral i
          _ -> error "Invalid cast."

runT funMap env (BinOp op xR yR) =
    do x <- runV funMap env xR
       y <- runV funMap env yR

       case (x, op, y) of
         (NInt a, BAdd, NInt b) -> return $ NInt (a + b)
         (NInt a, BSub, NInt b) -> return $ NInt (a - b)
         (NInt a, BMul, NInt b) -> return $ NInt (a * b)

         (NFloat a, BAdd, NFloat b) -> return $ NFloat (a + b)
         (NFloat a, BSub, NFloat b) -> return $ NFloat (a - b)
         (NFloat a, BMul, NFloat b) -> return $ NFloat (a * b)
         (NFloat a, BDiv, NFloat b) -> return $ NFloat (a / b)

         (NInt a, BSub, NFloat b) -> return $ NFloat ((fromIntegral a) - b)
         (NFloat a, BSub, NInt b) -> return $ NFloat (a - (fromIntegral b))
         (NInt a, BMul, NFloat b) -> return $ NFloat ((fromIntegral a) * b)
         (NFloat a, BMul, NInt b) -> return $ NFloat (a * (fromIntegral b))
         (NInt a, BAdd, NFloat b) -> return $ NFloat ((fromIntegral a) + b)
         (NFloat a, BAdd, NInt b) -> return $ NFloat (a + (fromIntegral b))

         (a, BEq, b) -> return $ NBool (a == b)
         (a, BNEq, b) -> return $ NBool (a /= b)
         (a, BLt, b) -> return $ NBool (a > b)
         (a, BSt, b) -> return $ NBool (a < b)
         (a, BLEq, b) -> return $ NBool (a >= b)
         (a, BSEq, b) -> return $ NBool (a <= b)

         (NBool a, BAnd, NBool b) -> return $ NBool (a && b)
         (NBool a, BOr, NBool b) -> return $ NBool (a || b)
         x -> error $ "Unknown operation: " ++ show x

runT funMap env (FunCall name) =
    apply funMap name []

runT funMap env (FunCall1 name a) =
    do aR <- runV funMap env a
       apply funMap name [aR]

runT funMap env (FunCall2 name a b) =
    do aR <- runV funMap env a
       bR <- runV funMap env b
       apply funMap name [aR, bR]

runT funMap env (FunCall3 name a b c) =
    do aR <- runV funMap env a
       bR <- runV funMap env b
       cR <- runV funMap env c
       apply funMap name [aR, bR, cR]

runT funMap env (FunCall4 name a b c d) =
    do aR <- runV funMap env a
       bR <- runV funMap env b
       cR <- runV funMap env c
       dR <- runV funMap env d
       apply funMap name [aR, bR, cR, dR]

runT _ env t = error $ "Not implemented:" ++ (prettyT t)

reqV (V (VarP p)) = p
reqV x = error $ "Require a variable, but got: " ++ (prettyV x)

getArgs funMap funName =
    case HM.lookup funName (fm_funs funMap) of
      Just (FunDefinition _ _ args _) ->
          map (\(DeclVar arg) -> reqV arg) args
      Nothing ->
          error $ "Function " ++ funName ++ " is not defined."

apply :: FunMap -> String -> [NXTVal] -> IO NXTVal
apply funMap funName inpVals =
    do funEnv <- emptyEnv

       case HM.lookup funName (fm_funs funMap) of
         Just fd ->
             do let args = getArgs funMap funName
                    vars = zip inpVals args
                mapM_ (\(src, tgt) ->
                           defineVar funEnv tgt src
                      ) vars
                runDef funMap funEnv fd
                getVar funEnv "__funReturnVal"
         Nothing ->
             (fm_apiCall funMap) funName inpVals
