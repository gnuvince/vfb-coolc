{-# LANGUAGE OverloadedStrings #-}

module Pretty where

import Prelude hiding (unlines)
import qualified Text.PrettyPrint as P
import qualified Data.ByteString.Lazy.Char8 as B


import AST

indent :: Int
indent = 4

class Pretty p where
    pretty :: p -> P.Doc

binOp :: String -> Expr a -> Expr a -> P.Doc
binOp op left right =
    P.parens (pretty left P.<+> P.text op P.<+> pretty right)

bytestr :: B.ByteString -> P.Doc
bytestr = P.text . B.unpack

instance Pretty (Expr a) where
    pretty (Assign x expr _) = bytestr x P.<+> "<-" P.<+> pretty expr
    pretty (MethodCall recv Nothing method params _) =
        P.cat [pretty recv, P.char '.', bytestr method,
               P.parens (P.cat (P.punctuate (P.text ", ") (map pretty params)))]
    pretty (MethodCall recv (Just ty) method params _) =
        P.cat [pretty recv, P.char '@', bytestr ty,
               P.char '.', bytestr method,
               P.parens (P.cat (P.punctuate (P.text ", ") (map pretty params)))]
    pretty (FunCall fun params _) =
        P.cat [bytestr fun,
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
                P.nest indent (P.vcat [pretty d P.<> P.char ';' | d <- decls]),
                P.text "in",
                P.nest indent (pretty expr)]
    pretty (Case expr branches _) =
        P.vcat [P.hsep [P.text "case", pretty expr, P.text "of"],
                P.nest indent (P.vcat [pretty b P.<> P.char ';' | b <- branches]),
                P.text "esac"]
    pretty (New ty _)  = P.text "new" P.<+> bytestr ty
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
    pretty (Id s _)    = bytestr s
    pretty (Int n _)   = P.int n
    pretty (Str s _)   = P.doubleQuotes (bytestr s)
    pretty (CTrue _)   = P.text "true"
    pretty (CFalse _)  = P.text "false"

instance Pretty (Program a) where
    pretty (Program classes _) =
        P.vcat [pretty c P.<> P.char ';' | c <- classes]

instance Pretty (Class a) where
    pretty (Class clsName inhCls features _) =
        P.vcat [P.hcat [P.text "class ", bytestr clsName, inherits, P.text " {"],
                P.nest indent (P.vcat [pretty f P.<> P.char ';' | f <- features]),
                P.char '}']
            where inherits = case inhCls of
                               Nothing -> P.empty
                               Just c  -> P.text " inherits" P.<+> bytestr c

instance Pretty (Param a) where
    pretty (Param name typ _) =
        P.hcat [bytestr name, P.text ": ", bytestr typ]

instance Pretty (Feature a) where
    pretty (VarDef name typ expr _) =
        P.hcat [bytestr name, P.char ':', bytestr typ, init]
         where init = case expr of
                        Nothing -> P.empty
                        Just e  -> P.text "<-" P.<+> pretty e

    pretty (MethodDef name params typ expr _) =
        P.vcat [P.cat [bytestr name, P.char '(',
                       P.cat (P.punctuate (P.text ", ") [pretty p | p <- params]),
                       P.char ')', retType, P.text " {"],
                P.nest indent (pretty expr),
                P.char '}']
            where retType = case typ of
                              Nothing -> P.empty
                              Just t  -> P.char ':' P.<+> bytestr t

instance Pretty (Decl a) where
    pretty (Decl name typ expr _) =
        P.hcat [bytestr name, P.text ": ", bytestr typ, init]
         where init = case expr of
                        Nothing -> P.empty
                        Just e  -> P.text " <- " P.<> pretty e

instance Pretty (CaseBranch a) where
    pretty (CaseBranch name typ expr _) =
        P.hcat [bytestr name, P.text ": ", bytestr typ, P.text " => ", pretty expr]
