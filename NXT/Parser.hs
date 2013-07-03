{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, OverloadedStrings #-}
module NXT.Parser
(
  pProg, runFile, runString
)
where

import NXT.Types

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)

import qualified Data.Text as T
import Data.Typeable

type Parser a = P (Str Char String LineColPos) a

-- | Program parser
pProg =
    pMany pFun

skipSpace p =
    pSpaces *> p <* pSpaces

condParser :: String -> Parser (T, [Stmt])
condParser which =
    mk <$> ((skipSpace (pToken which)) *> pParens pExpr) <*> pBraces (pMany pStmt) <* pSpaces
    where
      mk a b = (a, concat b)

pFun :: Parser FunDefinition
pFun =
    funDef <$> (skipSpace (pToken "function") *> skipSpace pIdent) <*> tupleParser (pIdent) <*> pBraces (pMany pStmt) <* pSpaces
    where
      funDef name args body = (FunDefinition (T.unpack name) "dynamic") (map (\x -> DeclVar "dynamic" (VarP $ T.unpack x)) args) $ concat body

pStmt :: Parser [Stmt]
pStmt =
    declAndAssign <$> (skipSpace (pToken "var") *> skipSpace pIdent) <*> (skipSpace (pSym '=') *> skipSpace pExpr) <* (pSym ';' *> pSpaces)
    <|>
    decl <$> (skipSpace (pToken "var") *> skipSpace pIdent) <* (pSym ';' *> pSpaces)
    <|>
    assign <$> skipSpace pIdent <*> (skipSpace (pSym '=') *> skipSpace pExpr) <* (pSym ';' *> pSpaces)
    <|>
    while <$> condParser "while"
    <|>
    mkRet <$> (skipSpace (pToken "return") *> skipSpace pExpr <* (pSym ';' *> pSpaces))
    <|>
    _if <$> (condParser "if") <*> pMany (condParser "else if") <*> ((skipSpace (pToken "else")) *> pBraces (pMany pStmt)) <* pSpaces
    <|>
    _if' <$> (condParser "if") <*> pMany (condParser "else if") <* pSpaces
    <|>
    eval <$> pExpr <* (pSym ';' *> pSpaces)
    where
      mkRet a = [FunReturn a]
      decl a = [DeclVar "dynamic" (VarP $ T.unpack a)]
      assign a b = [AssignVar (VarP $ T.unpack a) b]
      declAndAssign a b = [DeclVar "dynamic" (VarP $ T.unpack a), AssignVar (VarP $ T.unpack a) b]
      while (a, b) = [While a b]
      _if a b c = [mkIf a b $ concat c]
      _if' a b = [mkIf a b []]
      eval a = [Eval a]

mkIf :: (T, [Stmt]) -> [(T, [Stmt])] -> [Stmt] -> Stmt
mkIf (cond, body) (x:xs) elseBody =
    If cond body [(mkIf x xs elseBody)]
mkIf (cond, body) [] elseBody =
    If cond body elseBody

pMExpr :: Parser T
pMExpr =
    pInternal
    where
      pInternal = foldr pChainl pFactor [cmpops, addops, mulops]
      pFactor = pNoMath <|> pParens pInternal

      cmpops = anyOp [ ((qFun BEq), pSyms '=' '=')
                     , ((qFun BLt), pSym '>')
                     , ((qFun BSt), pSym '<')
                     , ((qFun BLEq),pSyms '>' '=')
                     , ((qFun BSEq),pSyms '<' '=')
                     , ((qFun BNEq),pSyms '!' '=')
                     ]

      addops = anyOp [ ((qFun BAdd),pSym '+')
                     , ((qFun BSub),pSym '-')
                     ]
      mulops = anyOp [ ((qFun BMul),pSym '*')
                     , ((qFun BDiv),pSym '/')
                     , ((qFun BOr), pSyms '|' '|')
                     , ((qFun BAnd),pSyms '&' '&')
                     ]

      pSyms a b = pSym a *> pSym b

      pOp (sem, p) = sem <$ p
      anyOp = pChoice . map pOp
      pChoice = foldr (<|>) pFail

      qFun x a b = BinOp x a b

pExpr =
    pNoMath
    <|>
    pMExpr

pNoMath =
    (VarP . T.unpack) <$> pIdent
    <|>
    fc <$> pIdent <*> tupleParser pExpr
    <|>
    BoolLit <$> pEnum
    <|>
    Lit <$> pInteger
    <|>
    (Rat . fromRational . toRational) <$> pDouble
    <|>
    StrLit <$> pQuotedString
    where
      fc n args = FunCall (T.unpack n) args

pIdent = pIdent' pLetter

pIdent' start =
    mkIdent <$> start <*> pMany (pLetter <|> pDigit)
    where
      mkIdent head tail = T.pack $ [head] ++ tail

-- | Helper Functions
runString :: Parser t -> String -> IO t
runString p inp =
    do let (r,msgs) = parse ((,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) (inp ++ "\n"))
       mapM_ (putStrLn . show) msgs
       return r

runFile :: Parser t -> String -> IO t
runFile p fname =
    do inp <- readFile fname
       r <- runString p (inp ++ "\n")
       return r
