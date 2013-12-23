{-# LANGUAGE OverloadedStrings #-}

{-|
Abstract syntax tree structure for COOL.  Every node type has a type
parameter @a@ that (I hope) will contain extra attributes such as
position information, type information, parent information, etc.
-}
module AST where

import qualified Data.ByteString.Lazy.Char8 as B

-- |For now, ids and type ids are simply bytestrings.
type Id   = B.ByteString
type Type = B.ByteString

-- |Node type for a Cool program. It contains a list of classes.
data Program a = Program {
      programClasses :: [Class a]
    , programAttr :: a
    } deriving (Show)

{-|
Node type for a Cool class. A class has a name, an optional
parent class (if Nothing, then we use it is a child of the
Object class), and a list of features, which are attribute
declarations and method definitions.
-}
data Class a = Class {
      className :: Type
    , classMaybeInheritedClass :: Maybe Type
    , classFeatures :: [Feature a]
    , classAttr :: a
    } deriving (Show)

classInheritedClass :: Class a -> Type
classInheritedClass cl =
    case classMaybeInheritedClass cl of
      Nothing -> "Object"
      Just t  -> t

{-|
A feature is either a method definition or an attribute definition.

A method definition has:

* a name;

* a list of 0 or more parameters;

* a return type;

* an expression.

An attribute definition has:

* a name;

* a type;

* an optional initialization expression.
-}
data Feature a  = MethodDef { methodName :: Id
                            , methodParams :: [Param a]
                            , methodType :: Type
                            , methodExpr :: Expr a
                            , methodAttr :: a
                            }
                | VarDef { varName :: Id
                         , varType :: Type
                         , varExpr :: Maybe (Expr a)
                         , varAttr :: a
                         }
                 deriving (Show)


-- |A parameter is declared in a method definition.
data Param a = Param {
      paramName :: Id
    , paramType :: Type
    , paramAttr :: a
    } deriving (Show)


-- |The expression nodes of Cool.
data Expr a = Assign { assignName :: Id
                     , assignExpr :: Expr a
                     , assignAttr :: a
                     }
            | MethodCall { mcallExpr :: Expr a
                         , mcallClass :: Maybe Type
                         , mcallName :: Id
                         , mcallParams :: [Expr a]
                         , mcallAttr :: a
                         }
            | FunCall { fcallName :: Id
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
            | Id { idName :: Id, idAttr :: a }
            | Int { intValue :: Int, intAttr :: a }
            | Str { strValue :: B.ByteString, strAttr :: a }
            | CTrue { ctrueAttr :: a }
            | CFalse { cfalseAttr :: a }
              deriving (Show)


-- |A declaration node used in a Let declaration.
data Decl a = Decl {
      declName :: Id
    , declType :: Type
    , declExpr :: Maybe (Expr a)
    , declAttr :: a
    } deriving (Show)

-- |A dynamic type dispatch branch used in a Case declaration.
data CaseBranch a = CaseBranch {
      branchName :: Id
    , branchType :: Type
    , branchExpr :: Expr a
    , branchAttr :: a
    } deriving (Show)


-- |A class that allows a single method to access the attributes of
-- all kinds of AST nodes.
class Attributable n where
    astAttr :: n a -> a

instance Attributable Program where
    astAttr = programAttr

instance Attributable Class where
    astAttr = classAttr

instance Attributable Feature where
    astAttr (MethodDef {methodAttr=a}) = a
    astAttr (VarDef {varAttr=a})       = a

instance Attributable Param where
    astAttr = paramAttr

instance Attributable Expr where
    astAttr (Assign {assignAttr=a})    = a
    astAttr (MethodCall {mcallAttr=a}) = a
    astAttr (FunCall {fcallAttr=a})    = a
    astAttr (IfThenElse {ifAttr=a})    = a
    astAttr (While {whileAttr=a})      = a
    astAttr (ExprList {listAttr=a})    = a
    astAttr (Let {letAttr=a})          = a
    astAttr (Case {caseAttr=a})        = a
    astAttr (New {newAttr=a})          = a
    astAttr (IsVoid {isVoidAttr=a})    = a
    astAttr (Add {addAttr=a})          = a
    astAttr (Sub {subAttr=a})          = a
    astAttr (Mul {mulAttr=a})          = a
    astAttr (Div {divAttr=a})          = a
    astAttr (Neg {negAttr=a})          = a
    astAttr (Lt  {ltAttr=a})           = a
    astAttr (Le  {leAttr=a})           = a
    astAttr (Eq  {eqAttr=a})           = a
    astAttr (Ge  {geAttr=a})           = a
    astAttr (Gt  {gtAttr=a})           = a
    astAttr (Not {notAttr=a})          = a
    astAttr (Id {idAttr=a})            = a
    astAttr (Int {intAttr=a})          = a
    astAttr (Str {strAttr=a})          = a
    astAttr (CTrue {ctrueAttr=a})      = a
    astAttr (CFalse {cfalseAttr=a})    = a

instance Attributable Decl where
    astAttr = declAttr

instance Attributable CaseBranch where
    astAttr = branchAttr
