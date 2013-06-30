{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module NXT.CodeGen where

import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Hashable
import GHC.Generics

exampleP =
    Prog "main" [exampleF]

exampleF =
    FunDef TVoid "main" [(TInt, "p1")
                        ,(TInt, "p2")
                        ] exampleExprs

exampleExprs =
    [ Decl TInt "result"
    , Assign "result" (Add (Var "p1") (Var "p2"))
    , Ret (Var "result")
    ]

-- | Program structure

type FunName = String
type VarName = String

data TypeDef
   = TStr
   | TInt
   | TBool
   | TDouble
   | TVoid
   deriving (Show, Eq, Enum, Generic)

instance Hashable TypeDef

data Prog
   = Prog FunName [FunDef]
   deriving (Show, Eq)

data FunDef
   = FunDef TypeDef FunName [(TypeDef, VarName)] [Expr]
   deriving (Show, Eq)

data Expr
   = Add Expr Expr
   | Sub Expr Expr
   | Mul Expr Expr
   | Div Expr Expr
   | Decl TypeDef VarName
   | Assign VarName Expr
   | Call FunName [Expr]
   | ApiCall FunName [Expr]
   | ILit Int
   | DLit Double
   | BLit Bool
   | SLit String
   | Var VarName
   | Ret Expr
   deriving (Show, Eq)

class HasConstName a where
    mkCName :: a -> String

instance HasConstName Int where
    mkCName val =
        "CI_" ++ (show val)

instance HasConstName Double where
    mkCName val =
        "CD_" ++ (show val)

instance HasConstName Bool where
    mkCName val =
        "CB_" ++ (show val)

instance HasConstName String where
    mkCName val =
        "CS_" ++ val

-- | Variable table
type InternalName = Int
type InternalRep = (InternalName, Maybe Expr)  -- internal name, default value
type VarTable = HM.HashMap (TypeDef, VarName) InternalRep

mkPTable :: Prog -> VarTable
mkPTable (Prog _ defs) =
    snd $ HM.foldlWithKey' mkNames (0, HM.empty) tmp
    where
      tmp = HM.fromList $ concatMap mkFTable defs
      mkNames (ctr, hm) k (_, expr) =
          (ctr+1, HM.insert k (ctr, expr) hm)

mkETable :: Expr -> Maybe ((TypeDef, VarName), InternalRep)
mkETable (Decl ty name) = Just ((ty, name), (0, Nothing))
mkETable v@(ILit val) = Just ((TInt, mkCName val), (0, Just $ v))
mkETable v@(DLit val) = Just ((TDouble, mkCName val), (0, Just $ v))
mkETable v@(BLit val) = Just ((TBool, mkCName val), (0, Just $ v))
mkETable v@(SLit val) = Just ((TStr, mkCName val), (0, Just $ v))
mkETable _ = Nothing

mkFTable :: FunDef -> [((TypeDef, VarName), InternalRep)]
mkFTable (FunDef _ _ args exprs) =
    argMap ++ exprMap
    where
      exprMap = catMaybes $ map mkETable exprs
      argMap =
          foldl (\xs arg ->
               ((arg, (0, Nothing)) : xs)
          ) [] args
