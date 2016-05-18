module StackCalc where

import Text.PrettyPrint
import Control.Monad.State.Lazy
import Data.Functor.Identity
import Control.Monad.Except


data Expr = Lit  Int
          | Add  Expr Expr
          | Mult Expr Expr
          | Neg  Expr

ppExpr :: Expr -> Doc
ppExpr (Lit n)      = int n
ppExpr (Add  e1 e2) = parens $ (ppExpr e1) <+> (text "+") <+> (ppExpr e2)
ppExpr (Mult e1 e2) = parens $ (ppExpr e1) <+> (text "*") <+> (ppExpr e2)
ppExpr (Neg e)      = (text "-") <> (parens $ ppExpr e)

test1 = Mult (Add (Lit 1) (Lit (-2))) (Neg (Add (Lit 3) (Lit 4)))
test2 = Add (Lit 2) (Neg $ Add (Lit 1) (Lit 1))

instance Show Expr where
  show = render . ppExpr

data StackToken = I Int | A | M | N deriving (Show,Eq)

type WorkingStack = [StackToken]
type CodeStack    = [StackToken]

compile :: Expr -> CodeStack
compile (Lit n)      = [I n]
compile (Add  e1 e2) = (compile e1) ++ (compile e2) ++ [A]
compile (Mult e1 e2) = (compile e1) ++ (compile e2) ++ [M]
compile (Neg  e    ) = (compile e) ++ [N]

------------------------------------------------
------------------------------------------------

type CalcState = (WorkingStack, CodeStack)

initialState :: Expr -> CalcState
initialState e = ([],compile e)

data Error = CodeStackEmpty 
           | WorkingStackEmpty 
           | OperationUndefined deriving (Show)

type Calc = ExceptT Error (StateT CalcState Identity)

runCalc :: Calc a -> CalcState -> CalcState
runCalc calc s = snd $ runIdentity $ runStateT (runExceptT calc) s

popCode :: Calc (Maybe StackToken)
popCode = do (w,c) <- get
             case c of []     -> do throwError CodeStackEmpty
                                    return Nothing
                       (t:ts) -> do put (w,ts)
                                    return $ Just t

popWorking :: Calc (Maybe StackToken)
popWorking = do (w,c) <- get
                case w of []     -> do throwError WorkingStackEmpty
                                       return Nothing
                          (t:ts) -> do put (ts,c)
                                       return $ Just t

pushWorking :: StackToken -> Calc ()
pushWorking t = do (w,c) <- get
                   put (t:w,c)
                   return ()

handleStackToken :: Maybe StackToken -> Calc ()
handleStackToken Nothing   = return ()
handleStackToken (Just t)  = case t of 
  I n -> pushWorking $ I n
  A   -> do t1 <- popWorking
            t2 <- popWorking
            case (t1,t2) of 
              (Just (I n), Just (I m)) -> pushWorking $ I (n+m)
              _                        -> do throwError OperationUndefined
                                             return ()
  M   -> do t1 <- popWorking
            t2 <- popWorking
            case (t1,t2) of 
              (Just (I n), Just (I m)) -> pushWorking $ I (n*m)
              _                        -> do throwError OperationUndefined
                                             return ()
  N   -> do t <- popWorking
            case t of 
              Just (I n) -> pushWorking $ I (-n)
              _          -> do throwError OperationUndefined
                               return ()

computationUnsafe :: Calc ()
computationUnsafe = do t <- popCode
                       handleStackToken t
                       computationUnsafe

halt :: Error -> Calc ()
halt _ = return ()

computation = catchError computationUnsafe halt
