module NXT.Inference
    ( runInference )
where

import NXT.Types

import qualified Data.HashMap.Strict as HM
import Data.IORef
import Control.Monad

data EnvContainer
   = EnvContainer
   { env_funs :: HM.HashMap String (IORef (Maybe (NXTType, [NXTType]), FunDefinition))
   , env_vars :: HM.HashMap String (IORef (Maybe NXTType))
   , env_const :: HM.HashMap String (T, NXTType)
   }

type Env = IORef EnvContainer

emptyEnv :: IO Env
emptyEnv = newIORef $ EnvContainer HM.empty HM.empty HM.empty

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
             case HM.lookup name (env_const container) of
               Just (_, v) ->
                   return $ Just v
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
                newBody <- fixStmts $ fd_body funDef
                (argTys, newArgs) <- foldM fixStmt ([], []) $ fd_args funDef
                let newDef = funDef { fd_body = newBody
                                    , fd_args = reverse $ newArgs
                                    , fd_type = if name == "main"
                                                then (if ty == NXTVoid
                                                      then "task"
                                                      else error "main function is not void!"
                                                     )
                                                else toCName ty
                                    }
                writeIORef funRef $ (Just (ty, argTys), newDef)

       where
         fixStmts x =
             do (_, x') <- foldM fixStmt ([], []) x
                return $ reverse x'

         fixStmt :: ([NXTType], [Stmt]) -> Stmt -> IO ([NXTType], [Stmt])
         fixStmt (inferred, allStmt) (DeclVar _ (VarP varname)) =
             do t <- getVarType envR varname
                case t of
                  Nothing ->
                      error $ "Variable `" ++ varname ++ "` in function `" ++ name ++ "` unused! Remove it."
                  Just ty ->
                      return $ (ty : inferred, DeclVar (toCName ty) (VarP varname) : allStmt)

         fixStmt (inferred, allStmt) (If t a b) =
             do t' <- fixT t
                a' <- fixStmts a
                b' <- fixStmts b
                return $ (inferred, If t' a' b' : allStmt)

         fixStmt (inferred, allStmt) (While t a) =
             do t' <- fixT t
                a' <- fixStmts a
                return $ (inferred, While t' a' : allStmt)

         fixStmt (inferred, allStmt) (AssignVar k v) =
             do v' <- fixT v
                return $ (inferred, AssignVar k v' : allStmt)

         fixStmt (inferred, allStmt) (Eval t) =
             do t' <- fixT t
                return $ (inferred, Eval t' : allStmt)

         fixStmt (inferred, allStmt) (FunReturn t) =
             do t' <- fixT t
                return $ (inferred, FunReturn t' : allStmt)

         fixT :: T -> IO T
         fixT v@(VarP name) =
             do m <- readIORef envR
                case HM.lookup name (env_const m) of
                  Just (expr, _) -> return $ expr
                  Nothing -> return $ v
         fixT (BinOp ty t1 t2) =
             do t1' <- fixT t1
                t2' <- fixT t2
                return $ BinOp ty t1' t2'
         fixT (FunCall name args) =
             do args' <- mapM fixT args
                return $ FunCall name args'
         fixT e = return e

copyFunEnv :: Env -> IO Env
copyFunEnv envR =
    do m <- readIORef envR
       newIORef $ EnvContainer (env_funs m) HM.empty (env_const m)

addConst :: Env -> String -> T -> NXTType -> IO ()
addConst envR name val ty =
    modifyIORef envR $ \c -> c { env_const = HM.insert name (val, ty) (env_const c) }

runInference :: [Definition] -> IO [FunDefinition]
runInference defs =
    do envR <- emptyEnv
       mapM_ (declFun envR) funs
       mapM_ (mkConst envR) consts
       mapM_ (mkLibDef envR) libDefs
       mapM (funInference envR) funs
    where
      mkLibDef envR (LibCallDefinition name extType argTypes) =
          do let fakeFunDef = FunDefinition name "" [] []
                 typeInfo = Just (extType, argTypes)
             ref <- newIORef (typeInfo, fakeFunDef)
             modifyIORef envR (\c -> c { env_funs = HM.insert name ref (env_funs c) })

      mkConst envR (ConstDefinition name expr) =
          do t <- exprInference envR expr
             addConst envR name expr t

      (libDefs, consts, funs) =
          foldl (\(lib, const, fun) def ->
                     case def of
                       PFun f ->
                           (lib, const, f : fun)
                       PConst c ->
                           (lib, c : const, fun)
                       PLib l ->
                           (l : lib, const, fun)
                ) ([], [], []) defs

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
