{
module Parser where

import qualified Data.ByteString.Lazy.Char8 as B

import Lexer
import AST
}


%name parseCool
%tokentype { Token }

%token
    'case'     { MkToken _ TCase }
    'class'    { MkToken _ TClass }
    'else'     { MkToken _ TElse }
    'esac'     { MkToken _ TEsac }
    'false'    { MkToken _ TFalse }
    'fi'       { MkToken _ TFi }
    'if'       { MkToken _ TIf }
    'in'       { MkToken _ TIn }
    'inherits' { MkToken _ TInherits }
    'isvoid'   { MkToken _ TIsvoid }
    'let'      { MkToken _ TLet }
    'loop'     { MkToken _ TLoop }
    'new'      { MkToken _ TNew }
    'not'      { MkToken _ TNot }
    'of'       { MkToken _ TOf }
    'pool'     { MkToken _ TPool }
    'then'     { MkToken _ TThen }
    'true'     { MkToken _ TTrue }
    'while'    { MkToken _ TWhile }
    ':'        { MkToken _ TColon }
    ';'        { MkToken _ TSemiColon }
    '@'        { MkToken _ TAt }
    '.'        { MkToken _ TDot }
    ','        { MkToken _ TComma }
    '<-'       { MkToken _ TAssign }
    '=>'       { MkToken _ TFatArrow }
    '+'        { MkToken _ TPlus }
    '-'        { MkToken _ TMinus }
    '*'        { MkToken _ TStar }
    '/'        { MkToken _ TSlash }
    '~'        { MkToken _ TTilde }
    '<'        { MkToken _ TLt }
    '<='       { MkToken _ TLe }
    '>'        { MkToken _ TGt }
    '>='       { MkToken _ TGe }
    '='        { MkToken _ TEq }
    '{'        { MkToken _ TLBrace }
    '}'        { MkToken _ TRBrace }
    '('        { MkToken _ TLParen }
    ')'        { MkToken _ TRParen }
    'int'      { MkToken _ (TInt _) }
    'str'      { MkToken _ (TString _) }
    'type'     { MkToken _ (TTypeId _) }
    'id'       { MkToken _ (TObjId _) }


%right    '<-'
%left     'not'
%nonassoc '<' '<=' '=' '>' '>='
%left     '+' '-'
%left     '*' '/'
%left     'isvoid'
%left     '~'
%left     '@'
%left     '.'

%%

program : classes                                       { Program $1 (astAttr (head $1)) }

classes : class ';'                                     { [$1] }
        | class ';' classes                             { $1 : $3 }

class   : 'class' 'type' '{' features '}'               { Class (tkType $2) Nothing $4 (tkPos $1) }
        | 'class' 'type' 'inherits' 'type' '{' features '}'
                                                        { Class (tkType $2) (Just (tkType $4)) $6 (tkPos $1) }

features : {- empty -}                                  { [] }
         | feature ';' features                         { $1 : $3 }

feature : 'id' '(' formals ')' ':' 'type' '{' expr '}' { MethodDef (tkId $1) $3 (tkType $6) $8 (tkPos $1) }
        | 'id' ':' 'type'                              { VarDef (tkId $1) (tkType $3) Nothing (tkPos $1) }
        | 'id' ':' 'type' '<-' expr                    { VarDef (tkId $1) (tkType $3) (Just $5) (tkPos $1) }

formals : {- empty -}                                   { [] }
        | formal                                        { [$1] }
        | formal ',' formals                            { $1 : $3 }

formal : 'id' ':' 'type'                                { Param (tkId $1) (tkType $3) (tkPos $1) }

exprCommaStar : {- empty -}                             { [] }
              | expr                                    { [$1] }
              | expr ',' exprCommaStar                  { $1 : $3 }

exprSemiPlus : expr ';'                                 { [$1] }
             | expr ';' exprSemiPlus                    { $1 : $3 }


expr : 'id' '<-' expr                                   { Assign (tkId $1) $3 (tkPos $1) }
     | expr '.' 'id' '(' exprCommaStar ')'              { MethodCall $1 Nothing (tkId $3) $5 (astAttr $1) }
     | expr '@' 'type' '.' 'id' '(' exprCommaStar ')'   { MethodCall $1 (Just (tkType $3)) (tkId $5) $7 (astAttr $1) }
     | 'id' '(' exprCommaStar ')'                       { FunCall (tkId $1) $3 (tkPos $1) }
     | 'if' expr 'then' expr 'else' expr 'fi'           { IfThenElse $2 $4 $6 (tkPos $1) }
     | 'while' expr 'loop' expr 'pool'                  { While $2 $4 (tkPos $1) }
     | 'let' letDecls 'in' expr                         { Let $2 $4 (tkPos $1) }
     | 'case' expr 'of' caseBranches 'esac'             { Case $2 $4 (tkPos $1) }
     | '{' exprSemiPlus '}'                             { ExprList $2 (tkPos $1) }
     | 'new' 'type'                                     { New (tkType $2) (tkPos $1) }
     | 'isvoid' expr                                    { IsVoid $2 (tkPos $1) }
     | expr '+' expr                                    { Add $1 $3 (astAttr $1) }
     | expr '-' expr                                    { Sub $1 $3 (astAttr $1) }
     | expr '*' expr                                    { Mul $1 $3 (astAttr $1) }
     | expr '/' expr                                    { Div $1 $3 (astAttr $1) }
     | '~' expr                                         { Neg $2 (tkPos $1) }
     | expr '<'  expr                                   { Lt $1 $3 (astAttr $1) }
     | expr '<=' expr                                   { Le $1 $3 (astAttr $1) }
     | expr '='  expr                                   { Eq $1 $3 (astAttr $1) }
     | expr '>'  expr                                   { Gt $1 $3 (astAttr $1) }
     | expr '>=' expr                                   { Ge $1 $3 (astAttr $1) }
     | 'not' expr                                       { Not $2 (tkPos $1) }
     | 'id'                                             { Id (tkId $1) (tkPos $1) }
     | 'int'                                            { Int (tkInt $1) (tkPos $1) }
     | 'str'                                            { Str (tkString $1) (tkPos $1) }
     | 'true'                                           { CTrue (tkPos $1) }
     | 'false'                                          { CFalse (tkPos $1) }
     | '(' expr ')'                                     { $2 }

letDecls : letDecl                                      { [$1] }
         | letDecl ',' letDecls                         { $1 : $3 }

letDecl : 'id' ':' 'type'                               { Decl (tkId $1) (tkType $3) Nothing (tkPos $1) }
        | 'id' ':' 'type' '<-' expr                     { Decl (tkId $1) (tkType $3) (Just $5) (tkPos $1) }

caseBranches : caseBranch ';'                           { [$1] }
             | caseBranch ';' caseBranches              { $1 : $3 }

caseBranch : 'id' ':' 'type' '=>' expr                  { CaseBranch (tkId $1) (tkType $3) $5 (tkPos $1) }

{

happyError :: [Token] -> a
happyError [] = error $ "parse error"
happyError (t:_) = error $ "parse error: " ++ show t

}
