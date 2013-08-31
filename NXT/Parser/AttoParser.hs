{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module NXT.Parser.AttoParser
    ( parseFile
    , pFullParser
    )
where

import NXT.Types
import qualified NXT.Parser.AttoExpr as E

import GHC.Float
import Data.Attoparsec.Text
import Data.Char (isSpace, isAlpha, isAlphaNum)
import qualified Data.Text as T
import Data.Maybe
import Numeric (readHex)

import Control.Applicative

import Prelude hiding (takeWhile)

-- | Helpers
parseFile :: FilePath -> IO (Either String [Definition])
parseFile fname =
    do inp <- readFile fname
       return $ parseOnly pFullParser (T.pack inp)

-- | Program parser
pFullParser =
    ((stripSpace $ many1 pDef) <* endOfInput)

pDef =
    pConst <|>
    pLibDef <|>
    pFun

pConst =
    mk <$> ((string "const" *> space_) *> pName <* (char '=' <* optSpace_)) <*> (stripSpace pExpr <* pStmtEnd)
    where
      mk a b = PConst $ ConstDefinition (T.unpack a) b

pFun =
    mk <$> ((string "function" *> space_) *> pName)
       <*> tupleP pName
       <*> (stripSpace $ (braced (char '{') (char '}') (many pStmt)))
       <* optSpace_
    where
      mk name args body =
          PFun $ FunDefinition (T.unpack name) "dynamic" (map (\x -> DeclVar "dynamic" (VarP $ T.unpack x)) args) $ concat body

pLibDef =
    mk <$> ((string "libDef" *> space_) *> (pTy <* char ':'))
       <*> pName
       <*> tupleP pTy
       <* pStmtEnd
    where
      mk extType name extArgTys =
          PLib $ LibCallDefinition (T.unpack name) extType extArgTys

pTy =
    const NXTString <$> string "string" <|>
    const NXTBool <$> string "bool" <|>
    const NXTInt <$> string "int" <|>
    const NXTFloat <$> string "float" <|>
    const NXTVoid <$> string "void"

pStmt = stripSpace pStmt'

pStmt' =
    pDecl <|>
    pDeclAndAssign <|>
    pAssign <|>
    pIf <|>
    pWhile <|>
    pReturn <|>
    pEval

pDecl =
    mk <$> ((string "var" *> space_) *> pName <* pStmtEnd)
    where
      mk a = [DeclVar "dynamic" (VarP $ T.unpack a)]

pDeclAndAssign =
    mk <$> ((string "var" *> space_) *> pName <* (char '=' <* optSpace_)) <*> (stripSpace pExpr <* pStmtEnd)
    where
      mk a b = [ DeclVar "dynamic" (VarP $ T.unpack a)
               , AssignVar (VarP $ T.unpack a) b
               ]

pAssign =
    mk <$> (pName <* (char '=' <* optSpace_)) <*> (stripSpace pExpr <* pStmtEnd)
    where
      mk a b = [ AssignVar (VarP $ T.unpack a) b ]

pWhile =
    mk <$> pCond "while"
    where
      mk (a, b) = [While a b]

pIf =
    mk <$> pCond "if"
       <*> many (pCond "else if")
       <*> (option [] $ (stripSpace (string "else")) *> (stripSpace $ (braced (char '{') (char '}') (many pStmt))) <* optSpace_)
    where
      mk a b c = [mkIf a b $ concat c]

      mkIf :: (T, [Stmt]) -> [(T, [Stmt])] -> [Stmt] -> Stmt
      mkIf (cond, body) (x:xs) elseBody =
          If cond body [(mkIf x xs elseBody)]
      mkIf (cond, body) [] elseBody =
          If cond body elseBody

pReturn =
    mk <$> ((string "return" *> space_) *> pExpr <* pStmtEnd)
    where
      mk a = [FunReturn a]

pEval =
    mk <$> pExpr <* pStmtEnd
    where
      mk a = [Eval a]

pCond :: T.Text -> Parser (T, [Stmt])
pCond condName =
    mk <$> ((stripSpace $ string condName) *> (braced (char '(') (char ')') pExpr))
       <*> (stripSpace $ (braced (char '{') (char '}') (many pStmt)))
       <* optSpace_
    where
      mk a b = (a, concat b)

pExpr =
    E.buildExpressionParser opTable pValExpr

pValExpr =
    braced (char '(') (char ')') pExpr <|>
    BoolLit <$> pBool <|>
    (StrLit . T.unpack) <$> stringP <|>
    mkLit <$> number <|>
    pFunCall <|>
    (VarP . T.unpack) <$> pName
    where
      mkLit n =
          case n of
            I int -> Lit $ fromIntegral int
            D dbl -> Rat $ double2Float dbl

pFunCall =
    fun <$> pName <*> tupleP pExpr
    where
      fun name args =
          FunCall (T.unpack name) args

liftOp op x = BinOp op x Void
liftOp2 op x y = BinOp op x y

opTable = [ [ --prefix (string "not" *> space_) (liftOp BNot)
            ]
          , [ binarySym "*" (liftOp2 BMul) E.AssocLeft
            , binarySym "/" (liftOp2 BDiv) E.AssocLeft
            ]
          , [ binarySym "+" (liftOp2 BAdd) E.AssocLeft
            , binarySym "-" (liftOp2 BSub) E.AssocLeft
            ]
          , [ binarySym "<" (liftOp2 BSt) E.AssocNone
            , binarySym ">" (liftOp2 BLt) E.AssocNone
            , binarySym "<=" (liftOp2 BSEq) E.AssocNone
            , binarySym ">=" (liftOp2 BLEq) E.AssocNone
            ]
          , [ binarySym "==" (liftOp2 BEq) E.AssocNone
            , binarySym "!=" (liftOp2 BNEq) E.AssocNone
            ]
          , [ binaryWord "and" (liftOp2 BAnd) E.AssocLeft
            ]
          , [ binaryWord "or" (liftOp2 BOr) E.AssocLeft
            ]
          ]
    where
        binary op fun assoc = E.Infix (fun <$ op <* optSpace_) assoc
        prefix op fun = E.Prefix (fun <$ op <* optSpace_)
        binarySym sym = binary (stripSpace $ string sym)
        binaryWord w = binary (between optSpace_ space_ $ string w)

pBool :: Parser Bool
pBool =
    const True <$> string "true" <|>
    const False <$> string "false"

stringP :: Parser T.Text
stringP = quotedString '"' <|> quotedString '\''

pStmtEnd = char ';' <* optSpace_

pName :: Parser T.Text
pName = stripSpace $ identP isAlpha isAlphaNum

stripSpace = between optSpace_ optSpace_

space_ = skipWhile1 isSpace
optSpace_ = skipWhile isSpace

between :: Parser a -> Parser b -> Parser c -> Parser c
between left right main = left *> main <* right

skipWhile1 pred = (() <$ takeWhile1 pred) <?> "skipWhile1"

identP :: (Char -> Bool) -> (Char -> Bool) -> Parser T.Text
identP first rest = T.cons <$> satisfy first <*> takeWhile rest

quotedString :: Char -> Parser T.Text
quotedString c = T.pack <$> between (char c) (char c) (many innerChar)
    where innerChar = char '\\' *> (escapeSeq <|> unicodeSeq)
                  <|> satisfy (`notElem` [c,'\\'])

escapeSeq :: Parser Char
escapeSeq = choice (zipWith decode "bnfrt\\\"'" "\b\n\f\r\t\\\"'")
    where decode c r = r <$ char c

unicodeSeq :: Parser Char
unicodeSeq = char 'u' *> (intToChar <$> decodeHexUnsafe <$> count 4 hexDigit)
    where intToChar = toEnum . fromIntegral

decodeHexUnsafe :: String -> Integer
decodeHexUnsafe hex = (head $ map fst $ readHex hex)

hexDigitUpper = satisfy (inClass "0-9A-F")
hexDigit = satisfy (inClass "0-9a-fA-F")

braced :: Parser l -> Parser r -> Parser a -> Parser a
braced l r = between (l *> optSpace_) (optSpace_ *> r)

listLike :: Parser l -> Parser r -> Parser s -> Parser a -> Parser [a]
listLike l r sep inner = braced l r (sepBy inner (stripSpace sep))

tupleP :: Parser p -> Parser [p]
tupleP = listLike (char '(') (char ')') (char ',')
