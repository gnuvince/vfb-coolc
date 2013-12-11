module AST where

type Type = String

data Program = Program {
      programClasses :: [Class]
    } deriving (Show)

data Class = Class {
      className :: Type
    , classInheritedClass :: Maybe Type
    , classFeatures :: [Feature]
    } deriving (Show)

data Feature = MethodDef { methodName :: String
                         , methodParams :: [Param]
                         , methodType :: Type
                         , methodExpr :: Expr
                         }
             | VarDef { varName :: String
                      , varType :: Type
                      , varExpr :: Maybe Expr
                      }
             deriving (Show)

data Param = Param {
      paramName :: String
    , paramType :: Type
    } deriving (Show)

data Expr = Assign { assignName :: String
                   , assignExpr :: Expr }
          | MethodCall { mcallExpr :: Expr
                       , mcallClass :: Maybe Type
                       , mcallName :: String
                       , mcallParams :: [Expr]
                       }
          | FunCall { fcallName :: String
                    , fcallParams :: [Expr]
                    }
          | IfThenElse { ifCond :: Expr
                       , ifThen :: Expr
                       , ifElse :: Expr
                       }
          | While { whileExpr :: Expr
                  , whileBody :: Expr
                  }
          | ExprList { listExprs :: [Expr] }
          | Let { letDecls :: [Decl]
                , letExpr :: Expr
                }
          | Case { caseExpr :: Expr
                 , caseBranches :: [CaseBranch]
                 }
          | New { newType :: Type }
          | IsVoid { isVoidExpr :: Expr}
          | Add { addLeft :: Expr, addRight :: Expr }
          | Sub { subLeft :: Expr, subRight :: Expr }
          | Mul { mulLeft :: Expr, mulRight :: Expr }
          | Div { divLeft :: Expr, divRight :: Expr }
          | Neg { negExpr :: Expr }
          | Lt  { ltLeft :: Expr, ltRight :: Expr }
          | Le  { leLeft :: Expr, leRight :: Expr }
          | Eq  { eqLeft :: Expr, eqRight :: Expr }
          | Ge  { geLeft :: Expr, geRight :: Expr }
          | Gt  { gtLeft :: Expr, gtRight :: Expr }
          | Not { notExpr :: Expr }
          | Id { idName :: String }
          | Int { intValue :: Int }
          | Str { strValue :: String }
          | CTrue
          | CFalse
            deriving (Show)



data Decl = Decl { declName :: String
                 , declType :: Type
                 , declExpr :: Maybe Expr }
          deriving (Show)

data CaseBranch = CaseBranch {
      branchName :: String
    , branchType :: Type
    , branchExpr :: Expr
    } deriving (Show)
