module AST where

{-
  Abstract syntax tree structure for our language.  Every type has a
  type parameter @a@ that (I hope) will contain extra attributes such as
  position information, type information, parent information, etc.
-}

-- Should this be a distinct type?
type Type = String

data Program a = Program {
      programClasses :: [Class]
    , programAttr :: a
    } deriving (Show)

data Class a = Class {
      className :: Type
    , classInheritedClass :: Maybe Type
    , classFeatures :: [Feature]
    , classAttr :: a
    } deriving (Show)

data Feature a  = MethodDef { methodName :: String
                            , methodParams :: [Param]
                            , methodType :: Type
                            , methodExpr :: Expr
                            , methodAttr :: a
                            }
                | VarDef { varName :: String
                         , varType :: Type
                         , varExpr :: Maybe Expr
                         , varAttr :: a
                         }
                 deriving (Show)

data Param a = Param {
      paramName :: String
    , paramType :: Type
    , paramAttr :: a
    } deriving (Show)

data Expr a = Assign { assignName :: String
                     , assignExpr :: Expr
                     , assignAttr :: a
                     }
            | MethodCall { mcallExpr :: Expr
                         , mcallClass :: Maybe Type
                         , mcallName :: String
                         , mcallParams :: [Expr]
                         , mcallAttr :: a
                         }
            | FunCall { fcallName :: String
                      , fcallParams :: [Expr]
                      , fcallAttr :: a
                      }
            | IfThenElse { ifCond :: Expr
                         , ifThen :: Expr
                         , ifElse :: Expr
                         , ifAttr :: a
                         }
            | While { whileExpr :: Expr
                    , whileBody :: Expr
                    , whileAttr :: a
                    }
            | ExprList { listExprs :: [Expr] , listAttr :: a }

            -- Should the Let constructor reflect that there should
            -- at least be one Decl?
            | Let { letDecls :: [Decl]
                  , letExpr :: Expr
                  , letAttr :: a
                  }
            | Case { caseExpr :: Expr
                   , caseBranches :: [CaseBranch]
                   , caseAttr :: a
                   }
            | New { newType :: Type, newAttr :: a }
            | IsVoid { isVoidExpr :: Exp, isVoidAttr :: a }
            | Add { addLeft :: Expr, addRight :: Expr, addAttr :: a }
            | Sub { subLeft :: Expr, subRight :: Expr, subAttr :: a }
            | Mul { mulLeft :: Expr, mulRight :: Expr, mulAttr :: a }
            | Div { divLeft :: Expr, divRight :: Expr, divAttr :: a }
            | Neg { negExpr :: Expr, negAttr :: a }
            | Lt  { ltLeft :: Expr, ltRight :: Expr, ltAttr :: a }
            | Le  { leLeft :: Expr, leRight :: Expr, leAttr :: a }
            | Eq  { eqLeft :: Expr, eqRight :: Expr, eqAttr :: a }
            | Ge  { geLeft :: Expr, geRight :: Expr, geAttr :: a }
            | Gt  { gtLeft :: Expr, gtRight :: Expr, gtAttr :: a }
            | Not { notExpr :: Expr, notAttr :: a }
            | Id { idName :: String, idAttr :: a }
            | Int { intValue :: Int, intAttr :: a }
            | Str { strValue :: String, strAttr :: a }
            | CTrue { ctrueAttr :: a }
            | CFalse { cfalseAttr :: a }
              deriving (Show)



data Decl a = Decl {
      declName :: String
    , declType :: Type
    , declExpr :: Maybe Expr
    , declAttr :: a
    } deriving (Show)

data CaseBranch a = CaseBranch {
      branchName :: String
    , branchType :: Type
    , branchExpr :: Expr
    , branchAttr :: a
    } deriving (Show)
