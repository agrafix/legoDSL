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
   If :: T -> [Stmt] -> [Stmt] -> Stmt
   While :: T -> [Stmt] -> Stmt
   DeclVar :: String -> T -> Stmt
   AssignVar :: T -> T -> Stmt
   Eval :: T -> Stmt
   FunReturn :: T -> Stmt

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

data T where
    Void :: T
    FunP :: String -> T
    VarP :: String -> T
    Lit  :: Int -> T
    Rat  :: Float -> T
    StrLit :: String -> T
    BoolLit :: Bool -> T
    BinOp :: BinOpT -> T -> T -> T
    CastOp :: String -> T -> T
    FunCall :: String -> T
    FunCall1 :: String -> T -> T
    FunCall2 :: String -> T -> T -> T
    FunCall3 :: String -> T -> T -> T -> T
    FunCall4 :: String -> T -> T -> T -> T -> T
    deriving (Typeable)

-- | wrapper for types
data V a = Typeable a => V T deriving (Typeable)

pack :: (Typeable a) => T -> V a
pack = V

unpack :: V a -> T
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

prettyT :: T -> String
prettyT (Void) = "";
prettyT (VarP p) = p
prettyT (FunP f) = f
prettyT (Lit i) = show i
prettyT (Rat i) = show i
prettyT (StrLit str) = show str
prettyT (BoolLit True) = "true"
prettyT (BoolLit False) = "false"
prettyT (CastOp target val) = "(" ++ target ++ "(" ++ (prettyT val) ++ "))"
prettyT (BinOp op x y) = "(" ++ (prettyT x) ++ (prettyOp op) ++ (prettyT y) ++ ")"
prettyT (FunCall name) = name ++ "()"
prettyT (FunCall1 name arg) = name ++ "(" ++ (prettyT arg) ++")"
prettyT (FunCall2 name a b) = name ++ "(" ++ (prettyT a) ++", " ++ (prettyT b) ++ ")"
prettyT (FunCall3 name a b c) = name ++ "(" ++ (prettyT a) ++", " ++ (prettyT b) ++ ", " ++ (prettyT c) ++ ")"
prettyT (FunCall4 name a b c d) = name ++ "(" ++ (prettyT a) ++", " ++ (prettyT b) ++ ", " ++ (prettyT c) ++ ", " ++ (prettyT d) ++ ")"
