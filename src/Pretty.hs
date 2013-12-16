module Pretty where

{-
import qualified Data.ByteString.Lazy.Char8 as B

import Data.List (intercalate)
import Prelude hiding (unlines)


import AST

unlines :: [String] -> String
unlines = intercalate "\n"

indent :: Int -> [String] -> [String]
indent ind lines = map (indentation++) lines
    where indentation = replicate ind ' '

ind4 :: [String] -> String
ind4 = unlines . indent 4


class Pretty p where
    pp :: p -> String

instance Pretty (Program a) where
    pp program = unlines [pp cl | cl <- programClasses program]

instance Pretty (Class a) where
    pp cls =
        unlines ["class " ++ clsName ++ clsInherit ++ " {"
                , unlines clsFeatures
                , "}"
                ]
        where clsName = className cls
              clsInherit = case classInheritedClass cls of
                             Nothing -> ""
                             Just c  -> " inherits " ++ c
              clsFeatures = map pp (classFeatures cls)

instance Pretty (Feature a) where
    pp (MethodDef name params ret expr _) =
        ind4 [concat [name, "(", intercalate ", " (map pp params), "): ", ret, " {"]
             , pp expr
             , "}"]



    pp (VarDef name typ expr _) =
        ind4 [concat [name, ": ", typ, initExpr]]
            where initExpr = case expr of
                               Nothing -> ""
                               Just e -> " <- " ++ pp e

instance Pretty (Param a) where
    pp (Param name typ _) =
        concat [name, ": ", typ]

instance Pretty (Expr a) where
    pp (Assign name expr _) = ind4 [concat [name, " <- ", pp expr]]

    pp (MethodCall recv targetCls method params _) =
        ind4 [concat ["(", pp recv, ")", targetClsStr, ".", method, intercalate "," (map pp params)]]
            where targetClsStr = case targetCls of
                                   Nothing -> ""
                                   Just c -> "@"++c

    pp (FunCall fname params _) =
        ind4 [concat [fname, "(", intercalate ", " (map pp params), ")"]]

    pp (IfThenElse cond thenBranch elseBranch _) =
        ind4 ["if " ++ pp cond ++  " then"
             , ind4 [pp thenBranch]
             , ind4 ["else"]
             , ind4 [pp elseBranch]
             , ind4 ["fi"]]

    pp (While expr body _) =
        ind4 [unlines ["while " ++ pp expr ++ " loop"
                      , pp body
                      , "pool"]]

    pp (ExprList exprs _) =
        ind4 [unlines ["{"
                      , unlines (map pp exprs)
                      , "}"]]

    pp (Let decls expr _) =
        ind4 [unlines ["let " ++ intercalate ", " (map pp decls) ++ " in"
                      , pp expr]]


    pp (Case expr branches _) =
        ind4 [unlines ["case " ++ pp expr ++ " of"
                      , unlines (map pp branches)]]

    pp (New typ _)     = "new " ++ typ
    pp (IsVoid expr _) = "isvoid " ++ pp expr
    pp (Add x y _)     = "(" ++ pp x ++ "+" ++ pp y ++ ")"
    pp (Sub x y _)     = "(" ++ pp x ++ "-" ++ pp y ++ ")"
    pp (Mul x y _)     = "(" ++ pp x ++ "*" ++ pp y ++ ")"
    pp (Div x y _)     = "(" ++ pp x ++ "/" ++ pp y ++ ")"
    pp (Neg x _)       = "~(" ++ pp x ++ ")"
    pp (Lt x y _)      = "(" ++ pp x ++ "<" ++ pp y ++ ")"
    pp (Le x y _)      = "(" ++ pp x ++ "<=" ++ pp y ++ ")"
    pp (Gt x y _)      = "(" ++ pp x ++ ">" ++ pp y ++ ")"
    pp (Ge x y _)      = "(" ++ pp x ++ ">=" ++ pp y ++ ")"
    pp (Eq x y _)      = "(" ++ pp x ++ "=" ++ pp y ++ ")"
    pp (Not x _)       = "not (" ++ pp x ++ ")"
    pp (Id x _)        = x
    pp (Int x _)       = show x
    pp (Str x _)       = show x
    pp (CTrue _)       = "true"
    pp (CFalse _)      = "false"


instance Pretty (Decl a) where
    pp (Decl name typ expr _) =
        ind4 [concat [name, ": ", typ, initExpr]]
            where initExpr = case expr of
                               Nothing -> ""
                               Just e -> " <- " ++ pp e



instance Pretty (CaseBranch a) where
    pp cb =
        ind4 [concat [branchName cb, ": ", branchType cb, " => ", pp (branchExpr cb)]]
-}
