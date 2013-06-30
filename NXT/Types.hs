{-# LANGUAGE GADTs, DeriveDataTypeable #-}
-- |
-- Module      : NXT.Types
-- Copyright   : Alexander Thiemann <mail@agrafix.net>
-- License     : BSD3
--
-- Maintainer  : Alexander Thiemann <mail@agrafix.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module NXT.Types where

import Data.Typeable
import Control.Monad.RWS (RWST)

type FunM = RWST String [Stmt] Int TopM
type TopM = RWST () [FunDefinition] Int IO

data Stmt where
   If :: (V Bool) -> [Stmt] -> [Stmt] -> Stmt
   While :: (V Bool) -> [Stmt] -> Stmt
   DeclVar :: Typeable a => (V a) -> Stmt
   AssignVar :: Typeable a => (V a) -> (V a) -> Stmt
   Eval :: Typeable a => (V a) -> Stmt
   FunReturn :: Typeable a => (V a) -> Stmt

data FunDefinition
   = FunDefinition
   { fd_name :: String
   , fd_type :: String
   , fd_args :: [Stmt]
   , fd_body :: [Stmt]
   }


data BinOpT
   = BAdd | BSub | BMul | BDiv | BEq | BNEq | BLt | BSt | BLEq | BSEq | BAnd | BOr
    deriving (Show, Eq, Typeable)

data T a where
    Void :: T ()
    FunP :: (Typeable a) => String -> T a
    VarP :: (Typeable a) => String -> T a
    Lit  :: (Typeable a) => Int -> T a
    Rat  :: (Typeable a) => Float -> T a
    StrLit :: (Typeable a) => String -> T a
    BoolLit :: (Typeable a) => Bool -> T a
    BinOp :: BinOpT -> (V c) -> (V c) -> T a
    CastOp :: (Typeable a) => String -> (V b) -> T a
    FunCall :: String -> T a
    FunCall1 :: String -> V a -> T b
    FunCall2 :: String -> V a -> V b -> T c
    FunCall3 :: String -> V a -> V b -> V c -> T d
    FunCall4 :: String -> V a -> V b -> V c -> V d -> T e
    deriving (Typeable)

-- | wrapper for types
data V a = Typeable a => V (T a) deriving (Typeable)

pack :: (Typeable a) => T a -> V a
pack = V

unpack :: V a -> T a
unpack (V a) = a

prettyOp :: BinOpT -> String
prettyOp BAdd = "+"
prettyOp BSub = "-"
prettyOp BMul = "*"
prettyOp BDiv = "/"
prettyOp BEq = "=="
prettyOp BLt = ">"
prettyOp BSt = "<"
prettyOp BLEq = ">="
prettyOp BSEq = "<="
prettyOp BNEq = "!="
prettyOp BOr = "||"
prettyOp BAnd = "&&"

prettyV :: V a -> String
prettyV = prettyT . unpack

prettyT :: T a -> String
prettyT (Void) = "";
prettyT (VarP p) = p
prettyT (FunP f) = f
prettyT (Lit i) = show i
prettyT (Rat i) = show i
prettyT (StrLit str) = show str
prettyT (BoolLit True) = "true"
prettyT (BoolLit False) = "false"
prettyT (CastOp target val) = "(" ++ target ++ "(" ++ (prettyV val) ++ "))"
prettyT (BinOp op x y) = "(" ++ (prettyV x) ++ (prettyOp op) ++ (prettyV y) ++ ")"
prettyT (FunCall name) = name ++ "()"
prettyT (FunCall1 name arg) = name ++ "(" ++ (prettyV arg) ++")"
prettyT (FunCall2 name a b) = name ++ "(" ++ (prettyV a) ++", " ++ (prettyV b) ++ ")"
prettyT (FunCall3 name a b c) = name ++ "(" ++ (prettyV a) ++", " ++ (prettyV b) ++ ", " ++ (prettyV c) ++ ")"
prettyT (FunCall4 name a b c d) = name ++ "(" ++ (prettyV a) ++", " ++ (prettyV b) ++ ", " ++ (prettyV c) ++ ", " ++ (prettyV d) ++ ")"
