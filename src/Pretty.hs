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
    pp (MethodDef name params ret expr) =
        printf "%s(%s): %s {\n%s\n}"
               name
               (intercalate ", " (map pp params))
               ret
               (pp expr)

    pp (VarDef name typ expr) =
        printf "%s: %s%s"
               name
               typ
               (case expr of
                  Nothing -> ""
                  Just e -> " <- " ++ pp e)

instance Pretty Param where
    pp (Param name typ) = name ++ ": " ++ typ

instance Pretty Expr where
    pp (Assign name expr) = printf "%s <- %s" name (pp expr)

    pp (MethodCall recv targetCls method params) =
        printf "(%s)%s.%s(%s)"
               (pp recv)
               (case targetCls of
                  Nothing -> ""
                  Just c -> "@"++c)
               method
               (intercalate ", " (map pp params))

    pp (FunCall fname params) =
        printf "%s(%s)"
               fname
               (intercalate ", " (map pp params))

    pp (IfThenElse cond then_ else_) =
        printf "if %s then %s else %s fi" (pp cond) (pp then_) (pp else_)

    pp (While expr body) =
        printf "while %s loop\n%s\npool" (pp expr) (pp body)

    pp (ExprList exprs) =
        printf "{\n%s}" (concat (map (\e -> pp e ++ ";\n") exprs))

    pp (Let decls expr) =
        printf "let %s in %s"
               (intercalate ", " (map pp decls))
               (pp expr)

    pp (Case expr branches) =
        printf "case %s of\n%s\nesac"
               (pp expr)
               (intercalate "\n" (map pp branches))

    pp (New typ) = "new " ++ typ
    pp (IsVoid expr) = "isvoid " ++ pp expr
    pp (Add x y) = "(" ++ pp x ++ "+" ++ pp y ++ ")"
    pp (Sub x y) = "(" ++ pp x ++ "-" ++ pp y ++ ")"
    pp (Mul x y) = "(" ++ pp x ++ "*" ++ pp y ++ ")"
    pp (Div x y) = "(" ++ pp x ++ "/" ++ pp y ++ ")"
    pp (Neg x)   = "~(" ++ pp x ++ ")"
    pp (Lt x y) = "(" ++ pp x ++ "<" ++ pp y ++ ")"
    pp (Le x y) = "(" ++ pp x ++ "<=" ++ pp y ++ ")"
    pp (Gt x y) = "(" ++ pp x ++ ">" ++ pp y ++ ")"
    pp (Ge x y) = "(" ++ pp x ++ ">=" ++ pp y ++ ")"
    pp (Eq x y) = "(" ++ pp x ++ "=" ++ pp y ++ ")"
    pp (Not x)  = "not (" ++ pp x ++ ")"
    pp (Id x) = x
    pp (Int x) = show x
    pp (Str x) = show x
    pp CTrue = "true"
    pp CFalse = "false"


instance Pretty Decl where
    pp (Decl name typ expr) =
        printf "%s: %s%s;"
               name
               typ
               (case expr of
                  Nothing -> ""
                  Just e -> " <- " ++ pp e)



instance Pretty CaseBranch where
    pp cb = printf "%s: %s => %s" (branchName cb) (branchType cb) (pp (branchExpr cb))
