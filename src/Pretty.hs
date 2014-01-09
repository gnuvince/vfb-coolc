{-# LANGUAGE OverloadedStrings #-}

module Pretty where

import Prelude hiding (unlines)
import qualified Text.PrettyPrint as P


import AST

indent :: Int
indent = 4

class Pretty p where
    pretty :: p -> P.Doc

binOp :: String -> Expr a -> Expr a -> P.Doc
binOp op left right =
    P.parens (pretty left P.<+> P.text op P.<+> pretty right)

instance Pretty (Expr a) where
    pretty (Assign x expr _) = P.text x P.<+> "<-" P.<+> pretty expr
    pretty (MethodCall recv cls method params _) =
        P.hcat [pretty recv, clsTarg,
                P.char '.', P.text method,
                P.parens (P.cat (P.punctuate (P.text ", ") (map pretty params)))]
            where clsTarg = case cls of
                              Nothing -> P.empty
                              Just t  -> P.char '@' P.<> P.text t
    pretty (FunCall fun params _) =
        P.hcat [P.text fun,
                P.parens (P.cat (P.punctuate (P.text ", ") (map pretty params)))]
    pretty (IfThenElse expr thenBranch elseBranch _) =
        P.vcat [P.hsep [P.text "if", pretty expr, P.text "then"],
                P.nest indent (pretty thenBranch),
                P.text "else",
                P.nest indent (pretty elseBranch),
                P.text "fi"]
    pretty (While expr body _) =
        P.vcat [P.hsep [P.text "while", pretty expr, P.text "loop"],
                P.nest indent (pretty body),
                P.text "pool"]
    pretty (ExprList exprs _) =
        P.vcat [P.text "{",
                P.nest indent (P.vcat [pretty e P.<> P.char ';' | e <- exprs]),
                P.text "}"]
    pretty (Let decls expr _) =
        P.vcat [P.text "let",
                P.nest indent (P.vcat (P.punctuate (P.char ',') (map pretty decls))),
                P.text "in",
                P.nest indent (pretty expr)]
    pretty (Case expr branches _) =
        P.vcat [P.hsep [P.text "case", pretty expr, P.text "of"],
                P.nest indent (P.vcat [pretty b P.<> P.char ';' | b <- branches]),
                P.text "esac"]
    pretty (New ty _)  = P.parens (P.text "new" P.<+> P.text ty)
    pretty (IsVoid expr _)  = P.text "isvoid" P.<+> P.parens (pretty expr)
    pretty (Add l r _) = binOp "+" l r
    pretty (Sub l r _) = binOp "-" l r
    pretty (Mul l r _) = binOp "*" l r
    pretty (Div l r _) = binOp "/" l r
    pretty (Lt l r _)  = binOp "<" l r
    pretty (Le l r _)  = binOp "<=" l r
    pretty (Eq l r _)  = binOp "=" l r
    pretty (Ge l r _)  = binOp ">=" l r
    pretty (Gt l r _)  = binOp ">" l r
    pretty (Neg e _)   = P.text "~" P.<> P.parens (pretty e)
    pretty (Not e _)   = P.text "not" P.<+> P.parens (pretty e)
    pretty (Id s _)    = P.text s
    pretty (Int n _)   = P.int n
    pretty (Str s _)   = P.doubleQuotes (P.text s)
    pretty (CTrue _)   = P.text "true"
    pretty (CFalse _)  = P.text "false"

instance Pretty (Program a) where
    pretty (Program classes _) =
        P.vcat [pretty c P.<> P.char ';' | c <- classes]

instance Pretty (Class a) where
    pretty (Class clsName inhCls features _) =
        P.vcat [P.hcat [P.text "class ", P.text clsName, inherits, P.text " {"],
                P.nest indent (P.vcat [pretty f P.<> P.char ';' | f <- features]),
                P.char '}']
            where inherits = case inhCls of
                               Nothing -> P.empty
                               Just c  -> P.text " inherits" P.<+> P.text c

instance Pretty (Param a) where
    pretty (Param name typ _) =
        P.hcat [P.text name, P.text ": ", P.text typ]

instance Pretty (Feature a) where
    pretty (VarDef name typ expr _) =
        P.hsep [P.text name, P.char ':', P.text typ, init]
         where init = case expr of
                        Nothing -> P.empty
                        Just e  -> P.text "<-" P.<+> pretty e

    pretty (MethodDef name params typ expr _) =
        P.vcat [P.cat [P.text name, P.char '(',
                       P.cat (P.punctuate (P.text ", ") [pretty p | p <- params]),
                       P.text "): ", P.text typ, P.text " {"],
                P.nest indent (pretty expr),
                P.char '}']

instance Pretty (Decl a) where
    pretty (Decl name typ expr _) =
        P.hcat [P.text name, P.text ": ", P.text typ, init]
         where init = case expr of
                        Nothing -> P.empty
                        Just e  -> P.text " <- " P.<> pretty e

instance Pretty (CaseBranch a) where
    pretty (CaseBranch name typ expr _) =
        P.hcat [P.text name, P.text ": ", P.text typ, P.text " => ", pretty expr]
