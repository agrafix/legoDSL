{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
-- |
-- Module      : NXT.Core
-- Copyright   : Alexander Thiemann <mail@agrafix.net>
-- License     : BSD3
--
-- Maintainer  : Alexander Thiemann <mail@agrafix.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module NXT.Core
    ( void, (#=), ($=), newVar, newFun, ret, ifThenElse, while, when
    , (<), (>), (<=), (>=), (==), (/=), (&&), (||)
    , (.!)
    , castFloat, castInt
    , true, false
    , mkProg, setMain, mkTree
    , callF, callF1, callF2, callF3, callF4
    , vCallF, vCallF1, vCallF2, vCallF3, vCallF4
    , defExt, mkLit
    , V, def
    )
where

import NXT.Types

import Data.List
import Data.Typeable
import Control.Monad hiding (void, when)
import Control.Monad.RWS hiding (void, when)

import Data.String

import Prelude hiding ((<), (>), (<=), (>=), (==), (/=), (&&), (||))
import qualified Prelude as P

instance (Num a, Typeable a) => Num (V a) where
    x + y = pack $ BinOp BAdd (unpack x) (unpack y)
    x * y = pack $ BinOp BMul (unpack x) (unpack y)
    x - y = pack $ BinOp BSub (unpack x) (unpack y)
    abs x = error "Not implemented"
    signum x = error "Not implemented"
    fromInteger = pack . Lit . fromInteger

instance Fractional (V Float) where
    x / y = pack $ BinOp BDiv (unpack x) (unpack y)
    fromRational = pack . Rat . fromRational

instance IsString (V String) where
    fromString str = pack . StrLit $ str

-- | logic and
(&&) :: V Bool -> V Bool -> V Bool
a && b = pack $ BinOp BAnd (unpack a) (unpack b)

-- | logic or
(||) :: V Bool -> V Bool -> V Bool
a || b = pack $ BinOp BOr (unpack a) (unpack b)

-- | compare two numbers for >
(>) :: (Num a, Typeable a) => V a -> V a -> V Bool
a > b = pack $ BinOp BLt (unpack a) (unpack b)

-- | compare two numbers for <
(<) :: (Num a, Typeable a) => V a -> V a -> V Bool
a < b = pack $ BinOp BSt (unpack a) (unpack b)

-- | compare two numbers for >=
(>=) :: (Num a, Typeable a) => V a -> V a -> V Bool
a >= b = pack $ BinOp BLEq (unpack a) (unpack b)

-- | compare two numbers for <=
(<=) :: (Num a, Typeable a) => V a -> V a -> V Bool
a <= b = pack $ BinOp BSEq (unpack a) (unpack b)

-- | the c++ == operation
(==) :: (Typeable a) => V a -> V a -> V Bool
a == b = pack $ BinOp BEq (unpack a) (unpack b)

-- | the c++ != operation
(/=) :: (Typeable a) => V a -> V a -> V Bool
a /= b = pack $ BinOp BNEq (unpack a) (unpack b)

-- | string concatination
(&) :: V String -> V String -> V String
a & b = pack $ FunCall "StrCat" [(unpack a), (unpack b)]

-- | Define an external function. Don't forget to add a type signature
defExt :: (Typeable a) => String -> V a
defExt name = pack $ FunP name

-- | Define an external literal. Don't forget to add a type signature
mkLit :: (Typeable a) => Int -> V a
mkLit lit = pack $ Lit lit

-- | Just "true"
true :: V Bool
true = pack $ BoolLit True

-- | Just "false"
false :: V Bool
false = pack $ BoolLit False

-- | Cast an int to a float
castFloat :: V Int -> V Float
castFloat x = pack $ CastOp "cast2float" $ unpack x


-- | Cast a float to an int
castInt :: V Float -> V Int
castInt x = pack $ CastOp "cast2int" $ unpack x

-- | Void. Do nothing
void :: V ()
void = pack $ Void

-- | Execute a void statement
(.!) :: (Typeable a) => V a -> FunM ()
(.!) stmt =
    tell [Eval $ unpack stmt]

-- | non monadic variable assignment
($=) :: (Typeable a) => V a -> V a -> FunM ()
varP $= nonMonadic =
    varP #= (return nonMonadic)

-- | monadic variable assignment (deprecated)
(#=) :: (Typeable a) => V a -> FunM (V a) -> FunM ()
v@(V (VarP pointer)) #= val =
    do rVal <- val
       tell [AssignVar (unpack v) (unpack rVal)]
(#=) _ _ = error "LH must be variable!"

newtype FResult a = FResult { unFResult :: a }

-- | define a functions return value
ret :: (Typeable a) => V a -> FunM (FResult (V a))
ret val =
    do tell [FunReturn $ unpack val]
       return $ FResult val

-- | the c++ while construct
while :: V Bool -> FunM () -> FunM ()
while cond loop =
    do orig <- get
       (_, _, loopOut) <- lift $ runRWST loop "loop" (orig + 200)
       tell [While (unpack cond) loopOut]

-- | when
when :: V Bool -> FunM () -> FunM ()
when cond t =
    ifThenElse cond t ((.!) void)

-- | the c++ if then else construct
ifThenElse :: V Bool -> FunM () -> FunM () -> FunM ()
ifThenElse cond t f =
    do orig <- get
       (_, _, tOut) <- lift $ runRWST t "ifTrue" (orig + 200)
       (_, _, fOut) <- lift $ runRWST f "ifFalse" (orig + 200)
       tell [If (unpack cond) tOut fOut]

getCType :: (Typeable a) => V a -> String
getCType a
    | (aT P.== strT) = "string"
    | (aT P.== boolT) = "bool"
    | (aT P.== intT) = "int"
    | (aT P.== integerT) = "int"
    | (aT P.== dblT) = "double"
    | (aT P.== floatT) = "float"
    | (aT P.== voidT) = "void"
    | otherwise = error $ "Unknown type " ++ show aT
    where
      aT = typeOf a
      voidT = typeOf $ (undefined :: V ())
      strT = typeOf $ (undefined :: V String)
      boolT = typeOf $ (undefined :: V Bool)
      integerT = typeOf $ (undefined :: V Integer)
      intT = typeOf $ (undefined :: V Int)
      dblT = typeOf $ (undefined :: V Double)
      floatT = typeOf $ (undefined :: V Float)

-- | declare a new variable, no type signature needed, will be inferred.
newVar :: forall a. (Typeable a)
       => FunM (V a)
newVar =
    do orig <- get
       let name = orig + 1
           var :: V a
           var = V (VarP $ "v" ++ show name)
       put name
       tell [dVar var]
       --tell ((getCType var) ++ " " ++ (prettyV var) ++ ";")
       return $ var

-- | declare a new function, no type signature needed, will be inferred.
newFun :: forall a. (Typeable a)
       => TopM (V a)
newFun =
    do orig <- get
       let name = orig + 1
           fun = FunP $ "f" ++ (show name)
       put name
       return $ pack fun

class (Typeable a) => FunDef a b | b -> a  where
    -- | Define a declared function
    def :: V a -> b -> TopM ()

instance forall a. (Typeable a) => FunDef a (FunM (FResult (V a))) where
    def name funAction =
        do (x, _, out) <- runRWST (funAction) (prettyV name) 0
           tell [FunDefinition (prettyV name) (getCType $ unFResult x) [] out]
           return ()

instance forall a b. (Typeable a, Typeable b) => FunDef (V a -> V b) (V a -> FunM (FResult (V b))) where
    def name funAction =
        do let var :: V a
               var = V (VarP "p1")
           (x, _, out) <- runRWST (funAction var) (prettyV name) 0
           tell [FunDefinition (prettyV name) (getCType $ unFResult x) [dVar var] out]
           return ()

instance forall a b c.  (Typeable a, Typeable b, Typeable c) => FunDef (V a -> V b -> V c) ((V a -> V b -> FunM (FResult (V c)))) where
    def name funAction =
        do let var :: V a
               var = V (VarP "p1")
               var2 :: V b
               var2 = V (VarP "p2")
           (x, _, out) <- runRWST (funAction var var2) (prettyV name) 0
           tell [FunDefinition (prettyV name) (getCType $ unFResult x) [dVar var, dVar var2] out]
           return ()

instance forall a b c d.  (Typeable a, Typeable b, Typeable c, Typeable d) => FunDef (V a -> V b -> V c -> V d) ((V a -> V b -> V c -> FunM (FResult (V d)))) where
    def name funAction =
        do let var :: V a
               var = V (VarP "p1")
               var2 :: V b
               var2 = V (VarP "p2")
               var3 :: V c
               var3 = V (VarP "p3")
           (x, _, out) <- runRWST (funAction var var2 var3) (prettyV name) 0
           tell [FunDefinition (prettyV name) (getCType $ unFResult x) [dVar var, dVar var2, dVar var3] out]
           return ()

instance forall a b c d e.  (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e) => FunDef (V a -> V b -> V c -> V d -> V e) ((V a -> V b -> V c -> V d -> FunM (FResult (V e)))) where
    def name funAction =
        do let var :: V a
               var = V (VarP "p1")
               var2 :: V b
               var2 = V (VarP "p2")
               var3 :: V c
               var3 = V (VarP "p3")
               var4 :: V d
               var4 = V (VarP "p4")
           (x, _, out) <- runRWST (funAction var var2 var3 var4) (prettyV name) 0
           tell [FunDefinition (prettyV name) (getCType $ unFResult x)
                                   [ dVar var, dVar var2, dVar var3, dVar var4] out]
           return ()

dVar v =
    DeclVar (getCType v) (unpack v)

-- | Call a function with no arguments
callF :: forall a. (Typeable a)
      => (V a) -> (V a)
callF fp@(V (FunP name)) =
    pack $ FunCall (prettyV fp) []
callF _ = error "Can only call functions!"

-- | Call a function with one argument
callF1 :: forall a b. (Typeable a, Typeable b)
      => (V (V a -> V b)) -> V a -> (V b)
callF1 fp@(V (FunP name)) arg =
    pack $ FunCall (prettyV fp) [unpack arg]
callF1 _ _ = error "Can only call functions!"

-- | Call a function with two arguments
callF2 :: forall a b c. (Typeable a, Typeable b, Typeable c)
      => (V (V a -> V b -> V c)) -> V a -> V b -> (V c)
callF2 fp@(V (FunP name)) arg1 arg2 =
    pack $ FunCall (prettyV fp) [(unpack arg1), (unpack arg2)]
callF2 _ _ _ = error "Can only call functions!"

-- | Call a function with tree arguments
callF3 :: forall a b c d. (Typeable a, Typeable b, Typeable c, Typeable d)
      => (V (V a -> V b -> V c -> V d)) -> V a -> V b -> V c -> V d
callF3 fp@(V (FunP name)) arg1 arg2 arg3 =
    pack $ FunCall (prettyV fp) [(unpack arg1), (unpack arg2), (unpack arg3)]
callF3 _ _ _ _ = error "Can only call functions!"

-- | Call a function with four arguments
callF4 :: forall a b c d e. (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e)
      => (V (V a -> V b -> V c -> V d -> V e)) -> V a -> V b -> V c -> V d -> V e
callF4 fp@(V (FunP name)) arg1 arg2 arg3 arg4 =
    pack $ FunCall (prettyV fp) [(unpack arg1), (unpack arg2), (unpack arg3), (unpack arg4)]
callF4 _ _ _ _ _ = error "Can only call functions!"

vCallF fp = (.!)$ callF fp
vCallF1 fp arg = (.!)$ callF1 fp arg
vCallF2 fp argA argB = (.!)$ callF2 fp argA argB
vCallF3 fp argA argB argC = (.!)$ callF3 fp argA argB argC
vCallF4 fp argA argB argC argD = (.!)$ callF4 fp argA argB argC argD

-- | Make Syntax-Tree
mkTree :: TopM a -> IO [FunDefinition]
mkTree prog =
    do (_, _, out) <- runRWST (mkStdLib >> prog) () 0
       return out

-- | Generate NXC code from your dsl
mkProg :: FilePath -> [FunDefinition] -> IO ()
mkProg filename prog =
    do writeFile filename $ concatMap prettyFD prog
       putStrLn "Finished generating NXC code ..."
       putStrLn $ "Written to " ++ filename

mkStdLib :: TopM ()
mkStdLib =
    do let var = (VarP "f")
           body = [FunReturn var]
           var2 = (VarP "i")
           body2 = [FunReturn var2]

       tell [FunDefinition "cast2int" "int"  [DeclVar "float" var] body]
       tell [FunDefinition "cast2float" "float" [DeclVar "int" var2] body2]

-- | Define a function as main task
setMain :: (Typeable a) => V a -> TopM ()
setMain fp@(V (FunP name)) =
    tell [FunDefinition "main" "task" [] [Eval $ unpack $ callF fp]]
