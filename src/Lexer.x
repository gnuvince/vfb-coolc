{
module Lexer (
               Token(..)
             , TokenClass(..)
             , tkPos
             , alexScanTokens
             )
where

import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "posn-bytestring"


$digit = 0-9
$alpha = [a-zA-Z_]
$alphanum = $alpha # $digit
$lower = [a-z_]
$upper = A-Z
$white = [\ \t\r\n\v\f]

@integer = 0 | [1-9][0-9]*
@typeid = $upper $alphanum*
@objid  = $lower $alphanum*
--@chars = [^ \" \0 \\] | \\[^ \0] | \\\n
@chars = [^\"] | \\\n

tokens :-

$white+ ;
"--".*\n  ;
"(*" .* "*)" ;

-- Keywords
case     { mkTk TCase }
class    { mkTk TClass }
else     { mkTk TElse }
esac     { mkTk TEsac }
false    { mkTk TFalse }
fi       { mkTk TFi }
if       { mkTk TIf }
in       { mkTk TIn }
inherits { mkTk TInherits }
isvoid   { mkTk TIsvoid }
let      { mkTk TLet }
loop     { mkTk TLoop }
new      { mkTk TNew }
not      { mkTk TNot }
of       { mkTk TOf }
pool     { mkTk TPool }
then     { mkTk TThen }
true     { mkTk TTrue }
while    { mkTk TWhile }


-- Literals
@integer        { \pos s -> mkTk (TInt (read $ B.unpack s)) pos s }
\" @chars* \"   { \pos s -> mkString pos s }

@typeid  { \pos s -> mkTk (TTypeId s) pos s }
@objid   { \pos s -> mkTk (TObjId s) pos s }

-- Symbols
":"  { mkTk TColon }
";"  { mkTk TSemiColon }
"@"  { mkTk TAt }
"."  { mkTk TDot }
","  { mkTk TComma }
"<-" { mkTk TAssign }
"=>" { mkTk TFatArrow }
"+"  { mkTk TPlus }
"-"  { mkTk TMinus }
"*"  { mkTk TStar }
"/"  { mkTk TSlash }
"~"  { mkTk TTilde }
"<"  { mkTk TLt }
"<=" { mkTk TLe }
">"  { mkTk TGt }
">=" { mkTk TGe }
"="  { mkTk TEq }
"{"  { mkTk TLBrace }
"}"  { mkTk TRBrace }
"("  { mkTk TLParen }
")"  { mkTk TRParen }



{
mkTk :: TokenClass -> AlexPosn -> B.ByteString -> Token
mkTk cls pos _ = MkToken pos cls

tkPos :: Token -> AlexPosn
tkPos (MkToken p _) = p

data Token = MkToken AlexPosn TokenClass
    deriving (Eq)

instance Show Token where
    show (MkToken (AlexPn _ l c) cl) =
        concat ["<", show cl, " ", show (l, c), ">"]

data TokenClass = TInt Int
                | TString B.ByteString
                | TTypeId B.ByteString
                | TObjId B.ByteString
                | TClass
                | TElse
                | TFalse
                | TFi
                | TIf
                | TIn
                | TInherits
                | TIsvoid
                | TLet
                | TLoop
                | TPool
                | TThen
                | TWhile
                | TCase
                | TEsac
                | TNew
                | TOf
                | TNot
                | TTrue
                | TColon
                | TSemiColon
                | TAt
                | TDot
                | TComma
                | TAssign
                | TFatArrow
                | TPlus
                | TMinus
                | TStar
                | TSlash
                | TTilde
                | TLt
                | TLe
                | TGt
                | TGe
                | TEq
                | TLBrace
                | TRBrace
                | TLParen
                | TRParen
           deriving (Show, Eq)


mkString :: AlexPosn -> B.ByteString -> Token
mkString pos s =
    let s' = B.init (B.tail s) in
    mkTk (TString s') pos s'
}
