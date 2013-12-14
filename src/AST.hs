module AST where

{-
  Abstract syntax tree structure for our language.  Every type has a
  type parameter @a@ that (I hope) will contain extra attributes such as
  position information, type information, parent information, etc.
-}

-- Should this be a distinct type?
type Type = String

data Program a = Program {
      programClasses :: [Class a]
    , programAttr :: a
    } deriving (Show)

data Class a = Class {
      className :: Type
    , classInheritedClass :: Maybe Type
    , classFeatures :: [Feature a]
    , classAttr :: a
    } deriving (Show)

data Feature a  = MethodDef { methodName :: String
                            , methodParams :: [Param a]
                            , methodType :: Type
                            , methodExpr :: Expr a
                            , methodAttr :: a
                            }
                | VarDef { varName :: String
                         , varType :: Type
                         , varExpr :: Maybe (Expr a)
                         , varAttr :: a
                         }
                 deriving (Show)

data Param a = Param {
      paramName :: String
    , paramType :: Type
    , paramAttr :: a
    } deriving (Show)

data Expr a = Assign { assignName :: String
                     , assignExpr :: Expr a
                     , assignAttr :: a
                     }
            | MethodCall { mcallExpr :: Expr a
                         , mcallClass :: Maybe Type
                         , mcallName :: String
                         , mcallParams :: [Expr a]
                         , mcallAttr :: a
                         }
            | FunCall { fcallName :: String
                      , fcallParams :: [Expr a]
                      , fcallAttr :: a
                      }
            | IfThenElse { ifCond :: Expr a
                         , ifThen :: Expr a
                         , ifElse :: Expr a
                         , ifAttr :: a
                         }
            | While { whileExpr :: Expr a
                    , whileBody :: Expr a
                    , whileAttr :: a
                    }
            | ExprList { listExprs :: [Expr a] , listAttr :: a }

            -- Should the Let constructor reflect that there should
            -- at least be one Decl?
            | Let { letDecls :: [Decl a]
                  , letExpr :: Expr a
                  , letAttr :: a
                  }
            | Case { caseExpr :: Expr a
                   , caseBranches :: [CaseBranch a]
                   , caseAttr :: a
                   }
            | New { newType :: Type, newAttr :: a }
            | IsVoid { isVoidExpr :: Expr a, isVoidAttr :: a }
            | Add { addLeft :: Expr a, addRight :: Expr a, addAttr :: a }
            | Sub { subLeft :: Expr a, subRight :: Expr a, subAttr :: a }
            | Mul { mulLeft :: Expr a, mulRight :: Expr a, mulAttr :: a }
            | Div { divLeft :: Expr a, divRight :: Expr a, divAttr :: a }
            | Neg { negExpr :: Expr a, negAttr :: a }
            | Lt  { ltLeft :: Expr a, ltRight :: Expr a, ltAttr :: a }
            | Le  { leLeft :: Expr a, leRight :: Expr a, leAttr :: a }
            | Eq  { eqLeft :: Expr a, eqRight :: Expr a, eqAttr :: a }
            | Ge  { geLeft :: Expr a, geRight :: Expr a, geAttr :: a }
            | Gt  { gtLeft :: Expr a, gtRight :: Expr a, gtAttr :: a }
            | Not { notExpr :: Expr a, notAttr :: a }
            | Id { idName :: String, idAttr :: a }
            | Int { intValue :: Int, intAttr :: a }
            | Str { strValue :: String, strAttr :: a }
            | CTrue { ctrueAttr :: a }
            | CFalse { cfalseAttr :: a }
              deriving (Show)



data Decl a = Decl {
      declName :: String
    , declType :: Type
    , declExpr :: Maybe (Expr a)
    , declAttr :: a
    } deriving (Show)

data CaseBranch a = CaseBranch {
      branchName :: String
    , branchType :: Type
    , branchExpr :: Expr a
    , branchAttr :: a
    } deriving (Show)
