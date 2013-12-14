module Pretty where

import Text.Printf (printf)
import Data.List (intercalate)

import AST


class Pretty p where
    pp :: p -> String

instance Pretty Program where
    pp program = unlines (map pp (programClasses program))

instance Pretty Class where
    pp cls = printf "class %s%s {\n%s\n}" clsName clsInherit clsFeatures
        where clsName = className cls
              clsInherit = case classInheritedClass cls of
                             Nothing -> ""
                             Just c  -> " inherits " ++ c
              clsFeatures = unlines (map pp (classFeatures cls))

instance Pretty Feature where
    pp (MethodDef name params ret expr _) =
        printf "%s(%s): %s {\n%s\n}"
               name
               (intercalate ", " (map pp params))
               ret
               (pp expr)

    pp (VarDef name typ expr _) =
        printf "%s: %s%s"
               name
               typ
               (case expr of
                  Nothing -> ""
                  Just e -> " <- " ++ pp e)

instance Pretty Param where
    pp (Param name typ _) = name ++ ": " ++ typ

instance Pretty Expr where
    pp (Assign name expr _) = printf "%s <- %s" name (pp expr)

    pp (MethodCall recv targetCls method params _) =
        printf "(%s)%s.%s(%s)"
               (pp recv)
               (case targetCls of
                  Nothing -> ""
                  Just c -> "@"++c)
               method
               (intercalate ", " (map pp params))

    pp (FunCall fname params _) =
        printf "%s(%s)"
               fname
               (intercalate ", " (map pp params))

    pp (IfThenElse cond thenBranch elseBranch _) =
        printf "if %s then %s else %s fi" (pp cond) (pp thenBranch) (pp elseBranch)

    pp (While expr body _) =
        printf "while %s loop\n%s\npool" (pp expr) (pp body)

    pp (ExprList exprs _) =
        printf "{\n%s}" (concat (map (\e -> pp e ++ ";\n") exprs))

    pp (Let decls expr _) =
        printf "let %s in %s"
               (intercalate ", " (map pp decls))
               (pp expr)

    pp (Case expr branches _) =
        printf "case %s of\n%s\nesac"
               (pp expr)
               (intercalate "\n" (map pp branches))

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


instance Pretty Decl where
    pp (Decl name typ expr _) =
        printf "%s: %s%s;"
               name
               typ
               (case expr of
                  Nothing -> ""
                  Just e -> " <- " ++ pp e)



instance Pretty CaseBranch where
    pp cb = printf "%s: %s => %s" (branchName cb) (branchType cb) (pp (branchExpr cb))
