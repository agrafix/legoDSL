module NXT.Inference
    ( runInference )
where

import NXT.Types

import qualified Data.HashMap.Strict as HM
import Data.IORef
import Control.Monad

-- allowed types:
-- string, bool, int, float, void

data NXTType
   = NXTString
   | NXTBool
   | NXTInt
   | NXTFloat
   | NXTVoid
   deriving (Show, Eq)

toCName :: NXTType -> String
toCName NXTString = "string"
toCName NXTBool = "bool"
toCName NXTInt = "int"
toCName NXTFloat = "float"
toCName NXTVoid = "void"

data EnvContainer
   = EnvContainer
   { env_funs :: HM.HashMap String (IORef (Maybe (NXTType, [NXTType]), FunDefinition))
   , env_vars :: HM.HashMap String (IORef (Maybe NXTType))
   }

type Env = IORef EnvContainer

emptyEnv :: IO Env
emptyEnv = newIORef $ EnvContainer HM.empty HM.empty

declVar :: Env -> String -> IO ()
declVar envR name =
    do varRef <- newIORef Nothing
       modifyIORef envR $ \c -> c { env_vars = HM.insert name varRef (env_vars c) }

declFun :: Env -> FunDefinition -> IO ()
declFun envR funDef =
     do funRef <- newIORef (Nothing, funDef)
        modifyIORef envR $ \c -> c { env_funs = HM.insert (fd_name funDef) funRef (env_funs c) }

getVarType :: Env -> String -> IO (Maybe NXTType)
getVarType envR name =
    do container <- readIORef envR
       case HM.lookup name (env_vars container) of
         Nothing ->
             error $ "Variable `" ++ name ++ "` not declared!"
         Just varRef ->
             readIORef varRef

setVarType :: Env -> String -> NXTType -> IO ()
setVarType envR name ty =
    do container <- readIORef envR
       case HM.lookup name (env_vars container) of
         Nothing ->
             error $ "Variable `" ++ name ++ "` not declared!"
         Just varRef ->
             writeIORef varRef $ Just ty

getFunType :: Env -> String -> IO (Maybe (NXTType, [NXTType]), FunDefinition)
getFunType envR name =
    do container <- readIORef envR
       case HM.lookup name (env_funs container) of
         Nothing ->
             error $ "Function `" ++ name ++ "` not defined!"
         Just funRef ->
             readIORef funRef

setFunType :: Env -> String -> NXTType -> IO ()
setFunType envR name ty =
    do container <- readIORef envR
       case HM.lookup name (env_funs container) of
         Nothing ->
             error $ "Function `" ++ name ++ "` not defined."
         Just funRef ->
             do (_, funDef) <- readIORef funRef
                (_, newBody) <- foldM fixStmt ([], []) $ fd_body funDef
                (argTys, newArgs) <- foldM fixStmt ([], []) $ fd_args funDef
                let newDef = funDef { fd_body = newBody
                                    , fd_args = newArgs
                                    , fd_type = toCName ty
                                    }
                writeIORef funRef $ (Just (ty, argTys), newDef)

       where
         fixStmt :: ([NXTType], [Stmt]) -> Stmt -> IO ([NXTType], [Stmt])
         fixStmt (inferred, allStmt) (DeclVar _ (VarP varname)) =
             do t <- getVarType envR varname
                case t of
                  Nothing ->
                      error $ "Variable `" ++ varname ++ "` in function `" ++ name ++ "` unused! Remove it."
                  Just ty ->
                      return $ (inferred ++ [ty], allStmt ++ [DeclVar (toCName ty) (VarP varname)])
         fixStmt (inferred, allStmt) stmt =
             return $ (inferred, allStmt ++ [stmt])

copyFunEnv :: Env -> IO Env
copyFunEnv envR =
    do m <- readIORef envR
       newIORef $ EnvContainer (env_funs m) HM.empty

runInference :: [FunDefinition] -> IO [FunDefinition]
runInference funs =
    do envR <- emptyEnv
       mapM_ (declFun envR) funs
       mapM (funInference envR) funs

funInference :: Env -> FunDefinition -> IO FunDefinition
funInference globalEnvR fun =
    do (myType, myDef) <- getFunType globalEnvR (fd_name fun)
       case myType of
         Just _ -> return myDef
         Nothing -> funInference' globalEnvR fun

funInference' :: Env -> FunDefinition -> IO FunDefinition
funInference' globalEnvR fun =
    do envR <- copyFunEnv globalEnvR
       mapM (assignVar envR) $ fd_args fun
       mapM (stmtInference envR) $ fd_body fun

       (myType, myDef) <- getFunType envR (fd_name fun)
       case myType of
         Nothing ->
             -- no return, assume void
             do setFunType envR (fd_name fun) NXTVoid
                (_, myNewDef) <- getFunType envR (fd_name fun)
                return myNewDef
         Just _ ->
             return myDef
    where
      assignVar envR (DeclVar _ (VarP name)) =
          declVar envR name
      assignVar _ _ =
          error "Invalid function parameter!"

      stmtInference envR (If cond true false) =
          do exprInference envR cond
             mapM_ (stmtInference envR) true
             mapM_ (stmtInference envR) false
      stmtInference envR (While cond body) =
          do exprInference envR cond
             mapM_ (stmtInference envR) body
      stmtInference envR (AssignVar (VarP name) val) =
          do t <- exprInference envR val
             setVarType envR name t
      stmtInference envR (DeclVar _ (VarP name)) =
          declVar envR name
      stmtInference envR (Eval expr) =
          do _ <- exprInference envR expr
             return ()
      stmtInference envR (FunReturn expr) =
          do r <- exprInference envR expr
             setFunType envR (fd_name fun) r
             return ()

      exprInference :: Env -> T -> IO NXTType
      exprInference envR Void =
          return NXTVoid
      exprInference envR (Lit _) =
          return NXTInt
      exprInference envR (Rat _) =
          return NXTFloat
      exprInference envR (StrLit _) =
          return NXTString
      exprInference envR (BoolLit _) =
          return NXTBool
      exprInference envR (VarP name) =
          do varT <- getVarType envR name
             case varT of
               Nothing ->
                   error $ "Variable `" ++ name ++ "` used before assigned value."
               Just t ->
                   return t
      exprInference envR (FunCall funName args) =
          do (_, myDef) <- getFunType envR funName
             -- run inference for function
             _ <- funInference envR myDef
             (Just (myType, argTypes), _) <- getFunType envR funName
             -- type check
             callTypes <- mapM (exprInference envR) args
             when (callTypes /= argTypes) $ error $ "Function " ++ funName ++ " called with args of wrong type."
             return myType

      exprInference envR (BinOp opType left right) =
          case HM.lookup opType opTable of
            Nothing ->
                error $ "Unkown operator : " ++ show opType
            Just possibleVals ->
                do let isAllowed t =
                           if t `elem` possibleVals
                           then t
                           else error $ "In the BinOp " ++ (show opType) ++ " there is a type error!"
                       checkInferReturn ty (VarP varname) =
                           do setVarType envR varname ty
                              return $ isAllowed ty

                   mlType <- safeInference envR left
                   mrType <- safeInference envR right

                   case (mlType, mrType) of
                     (Just a, Just b) ->
                         if a == b
                         then return $ isAllowed a
                         else error $ "BinOp (" ++ (show opType) ++") type error: " ++ (show a) ++ " and " ++ (show b)
                     (Nothing, Just b) ->
                         checkInferReturn b left
                     (Just a, Nothing) ->
                         checkInferReturn a right
                     (Nothing, Nothing) ->
                         do checkInferReturn (head possibleVals) left
                            checkInferReturn (head possibleVals) right


      exprInference envR expr =
          error $ "Can't do inference for " ++ (show expr)

      safeInference :: Env -> T -> IO (Maybe NXTType)
      safeInference envR (VarP name) =
          getVarType envR name

      safeInference envR expr =
          do x <- exprInference envR expr
             return $ Just x


-- BAdd | BSub | BMul | BDiv | BEq | BNEq | BLt | BSt | BLEq | BSEq | BAnd | BOr
opTable =
    HM.fromList $
    [ ( BAdd, [ NXTInt, NXTFloat ] )
    , ( BSub, [ NXTInt, NXTFloat ] )
    , ( BMul, [ NXTInt, NXTFloat ] )
    , ( BDiv, [ NXTInt, NXTFloat ] )
    , ( BEq, [ NXTVoid, NXTInt, NXTFloat, NXTString, NXTBool ] )
    , ( BNEq, [ NXTVoid, NXTInt, NXTFloat, NXTString, NXTBool ] )
    , ( BLt, [ NXTInt, NXTFloat ] )
    , ( BSt, [ NXTInt, NXTFloat ] )
    , ( BLEq, [ NXTInt, NXTFloat ] )
    , ( BSEq, [ NXTInt, NXTFloat ] )
    , ( BAnd, [ NXTBool ])
    , ( BOr, [ NXTBool ])
    ]

{-
data Stmt
   = If T [Stmt] [Stmt]
   | While T [Stmt]
   | DeclVar String T
   | AssignVar T T
   | Eval T
   | FunReturn T
   deriving Show
-}

{-
data T
   = Void
   | FunP String
   | VarP String
   | Lit Int
   | Rat Float
   | StrLit String
   | BoolLit Bool
   | BinOp BinOpT T T
   | CastOp String T
   | FunCall String [T]
    deriving (Typeable, Show)-}
