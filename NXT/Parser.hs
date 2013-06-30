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
    funDef <$> ((pToken "function") *> skipSpace pIdent) <*> tupleParser (pIdent) <*> pBraces pStmt <* pSpaces
    where
      funDef name args body = (FunDefinition (T.unpack name) "dynamic") (map (\x -> DeclVar "dynamic" (VarP $ T.unpack x)) args) body

pStmt :: Parser [Stmt]
pStmt =
    decl <$> skipSpace pIdent <* (pSym ';' *> pSpaces)
    <|>
    assign <$> skipSpace pIdent <*> (skipSpace (pSym '=') *> skipSpace pExpr) <* (pSym ';' *> pSpaces)
    <|>
    declAndAssign <$> skipSpace pIdent <*> (skipSpace (pSym '=') *> skipSpace pExpr) <* (pSym ';' *> pSpaces)
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

pMTerm :: Parser T
pMTerm =
    quickFun BMul <$> (skipSpace pExpr <* pSym '*') <*> skipSpace pExpr
    <|>
    quickFun BDiv <$>  (skipSpace pExpr <* pSym '/') <*> skipSpace pExpr
    <|>
    pExpr

pMExpr :: Parser T
pMExpr =
    quickFun BAdd <$> (skipSpace pMTerm <* pSym '+') <*> skipSpace pMTerm
    <|>
    quickFun BSub <$> (skipSpace pMTerm <* pSym '-') <*> skipSpace pMTerm
    <|>
    quickFun BEq <$> (skipSpace pMTerm <* pSym '=' <* pSym '=') <*> skipSpace pMTerm
    <|>
    quickFun BNEq <$> (skipSpace pMTerm <* pSym '!' <* pSym '=') <*> skipSpace pMTerm
    <|>
    quickFun BSEq <$> (skipSpace pMTerm <* pSym '<' <* pSym '=') <*> skipSpace pMTerm
    <|>
    quickFun BSEq <$> (skipSpace pMTerm <* pSym '>' <* pSym '=') <*> skipSpace pMTerm
    <|>
    quickFun BSt <$> (skipSpace pMTerm <* pSym '<') <*> skipSpace pMTerm
    <|>
    quickFun BLt <$> (skipSpace pMTerm <* pSym '>') <*> skipSpace pMTerm

quickFun name a b =
    BinOp name a b

pMFact =
    pParens pMExpr

pExpr =
    fc <$> pIdent <*> tupleParser pExpr
    <|>
    pMFact
    <|>
    StrLit <$> pQuotedString
    <|>
    (Rat . fromRational . toRational) <$> pDouble
    <|>
    BoolLit <$> pEnum
    <|>
    Lit <$> pInteger
    <|>
    (VarP . T.unpack) <$> pIdent
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
