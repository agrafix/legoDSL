{-# LANGUAGE GADTs #-}
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
import NXT.Interpretation

import Data.List
import Data.Typeable
import Control.Monad hiding (void, when)
import Control.Monad.RWS hiding (void, when)

import Data.String

import Prelude hiding ((<), (>), (<=), (>=), (==), (/=), (&&), (||))
import qualified Prelude as P

prettyFD :: FunDefinition -> String
prettyFD (FunDefinition name ty args body) =
    ty ++ " " ++ name ++ "(" ++ arglist ++ ") {" ++ (concatMap prettyStmt body) ++ "}"
    where
      arglist = intercalate ", " $ map (\(DeclVar arg) -> ((getCType arg) ++ " " ++ (prettyV arg))) args

prettyStmt :: Stmt -> String
prettyStmt (If cond t f) =
    "if (" ++ (prettyV cond) ++ ") {"
    ++ (concatMap prettyStmt t) ++ "}"
    ++ (case f of
          [] -> ""
          _ -> "else { " ++ (concatMap prettyStmt f) ++ "}"
       )
prettyStmt (While cond loop) =
    "while (" ++ (prettyV cond) ++ ") {"
    ++ (concatMap prettyStmt loop) ++ "}"
prettyStmt (DeclVar var@(V (VarP name))) =
    (getCType var) ++ " " ++ (prettyV var) ++ ";"
prettyStmt (AssignVar v@(V (VarP pointer)) val) =
    (prettyV v) ++ " = " ++ (prettyV val) ++ ";"
prettyStmt (Eval something) =
    (prettyV something) ++ ";"
prettyStmt (FunReturn val) =
    "return " ++ prettyV val ++ ";"

prettyStmt _ = error "Error: Invalid syntax tree"

instance (Num a, Typeable a) => Num (V a) where
    x + y = pack $ BinOp BAdd x y
    x * y = pack $ BinOp BMul x y
    x - y = pack $ BinOp BSub x y
    abs x = error "Not implemented"
    signum x = error "Not implemented"
    fromInteger = pack . Lit . fromInteger

instance Fractional (V Float) where
    x / y = pack $ BinOp BDiv x y
    fromRational = pack . Rat . fromRational

instance IsString (V String) where
    fromString str = pack . StrLit $ str

-- | logic and
(&&) :: V Bool -> V Bool -> V Bool
a && b = pack $ BinOp BAnd a b

-- | logic or
(||) :: V Bool -> V Bool -> V Bool
a || b = pack $ BinOp BOr a b

-- | compare two numbers for >
(>) :: (Num a) => V a -> V a -> V Bool
a > b = pack $ BinOp BLt a b

-- | compare two numbers for <
(<) :: (Num a) => V a -> V a -> V Bool
a < b = pack $ BinOp BSt a b

-- | compare two numbers for >=
(>=) :: (Num a) => V a -> V a -> V Bool
a >= b = pack $ BinOp BLEq a b

-- | compare two numbers for <=
(<=) :: (Num a) => V a -> V a -> V Bool
a <= b = pack $ BinOp BSEq a b

-- | the c++ == operation
(==) :: V a -> V a -> V Bool
a == b = pack $ BinOp BEq a b

-- | the c++ != operation
(/=) :: V a -> V a -> V Bool
a /= b = pack $ BinOp BNEq a b

-- | string concatination
(&) :: V String -> V String -> V String
a & b = pack $ FunCall2 "StrCat" a b

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
castFloat x = pack $ CastOp "cast2float" x


-- | Cast a float to an int
castInt :: V Float -> V Int
castInt x = pack $ CastOp "cast2int" x

-- | Void. Do nothing
void :: V ()
void = pack $ Void

-- | Execute a void statement
(.!) :: (Typeable a) => V a -> FunM ()
(.!) stmt =
    tell [Eval stmt]

-- | non monadic variable assignment
($=) :: (Typeable a) => V a -> V a -> FunM ()
varP $= nonMonadic =
    varP #= (return nonMonadic)

-- | monadic variable assignment (deprecated)
(#=) :: (Typeable a) => V a -> FunM (V a) -> FunM ()
v@(V (VarP pointer)) #= val =
    do rVal <- val
       tell [AssignVar v rVal]
(#=) _ _ = error "LH must be variable!"

newtype FResult a = FResult { unFResult :: a }

-- | define a functions return value
ret :: (Typeable a) => V a -> FunM (FResult (V a))
ret val =
    do tell [FunReturn val]
       return $ FResult val

-- | the c++ while construct
while :: V Bool -> FunM () -> FunM ()
while cond loop =
    do orig <- get
       (_, _, loopOut) <- lift $ runRWST loop "loop" (orig + 200)
       tell [While cond loopOut]

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
       tell [If cond tOut fOut]

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
       tell [DeclVar var]
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

funTpl funN fType fArgs fBody =
    do tell $ funHeader ++ ";"
       tell $ funHeader ++ " { " ++ (concatMap prettyStmt fBody) ++ " }"
    where
      funHeader = fType ++ " " ++ funN
                  ++ "(" ++ (intercalate "," $ map (\(t, v) -> t ++ " " ++ v) fArgs) ++ ")"

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
           tell [FunDefinition (prettyV name) (getCType $ unFResult x) [DeclVar var] out]
           return ()

instance forall a b c.  (Typeable a, Typeable b, Typeable c) => FunDef (V a -> V b -> V c) ((V a -> V b -> FunM (FResult (V c)))) where
    def name funAction =
        do let var :: V a
               var = V (VarP "p1")
               var2 :: V b
               var2 = V (VarP "p2")
           (x, _, out) <- runRWST (funAction var var2) (prettyV name) 0
           tell [FunDefinition (prettyV name) (getCType $ unFResult x) [DeclVar var, DeclVar var2] out]
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
           tell [FunDefinition (prettyV name) (getCType $ unFResult x) [DeclVar var, DeclVar var2, DeclVar var3] out]
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
           tell [FunDefinition (prettyV name) (getCType $ unFResult x) [DeclVar var, DeclVar var2, DeclVar var3, DeclVar var4] out]
           return ()

-- | Call a function with no arguments
callF :: forall a. (Typeable a)
      => (V a) -> (V a)
callF fp@(V (FunP name)) =
    pack $ FunCall (prettyV fp)
callF _ = error "Can only call functions!"

-- | Call a function with one argument
callF1 :: forall a b. (Typeable a, Typeable b)
      => (V (V a -> V b)) -> V a -> (V b)
callF1 fp@(V (FunP name)) arg =
    pack $ FunCall1 (prettyV fp) arg
callF1 _ _ = error "Can only call functions!"

-- | Call a function with two arguments
callF2 :: forall a b c. (Typeable a, Typeable b, Typeable c)
      => (V (V a -> V b -> V c)) -> V a -> V b -> (V c)
callF2 fp@(V (FunP name)) arg1 arg2 =
    pack $ FunCall2 (prettyV fp) arg1 arg2
callF2 _ _ _ = error "Can only call functions!"

-- | Call a function with tree arguments
callF3 :: forall a b c d. (Typeable a, Typeable b, Typeable c, Typeable d)
      => (V (V a -> V b -> V c -> V d)) -> V a -> V b -> V c -> V d
callF3 fp@(V (FunP name)) arg1 arg2 arg3 =
    pack $ FunCall3 (prettyV fp) arg1 arg2 arg3
callF3 _ _ _ _ = error "Can only call functions!"

-- | Call a function with four arguments
callF4 :: forall a b c d e. (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e)
      => (V (V a -> V b -> V c -> V d -> V e)) -> V a -> V b -> V c -> V d -> V e
callF4 fp@(V (FunP name)) arg1 arg2 arg3 arg4 =
    pack $ FunCall4 (prettyV fp) arg1 arg2 arg3 arg4
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
    do let var :: V Float
           var = V (VarP "f")
           body = [FunReturn var]

           var2 :: V Int
           var2 = V (VarP "i")
           body2 = [FunReturn var2]

       tell [FunDefinition "cast2int" "int"  [DeclVar var] body]
       tell [FunDefinition "cast2float" "float" [DeclVar var2] body2]

-- | Define a function as main task
setMain :: V a -> TopM ()
setMain fp@(V (FunP name)) =
    tell [FunDefinition "main" "task" [] [Eval $ callF fp]]
