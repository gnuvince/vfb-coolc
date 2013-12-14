{
module Lexer (
               Token(..)
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

@integer = 0 | \-?[1-9][0-9]*
@typeid = $upper $alphanum*
@objid  = $lower $alphanum*
--@chars = [^ \" \0 \\] | \\[^ \0] | \\\n
@chars = [^\"] | \\\n

tokens :-

$white+ ;
"--".*\n  ;
"(*" .* "*)" ;

-- Keywords
case     { \pos _ -> TCase pos }
class    { \pos _ -> TClass pos }
else     { \pos _ -> TElse pos }
esac     { \pos _ -> TEsac pos }
false    { \pos _ -> TFalse pos }
fi       { \pos _ -> TFi pos }
if       { \pos _ -> TIf pos }
in       { \pos _ -> TIn pos }
inherits { \pos _ -> TInherits pos }
isvoid   { \pos _ -> TIsvoid pos }
let      { \pos _ -> TLet pos }
loop     { \pos _ -> TLoop pos }
new      { \pos _ -> TNew pos }
not      { \pos _ -> TNot pos }
of       { \pos _ -> TOf pos }
pool     { \pos _ -> TPool pos }
then     { \pos _ -> TThen pos }
true     { \pos _ -> TTrue pos }
while    { \pos _ -> TWhile pos }


-- Literals
@integer        { \pos s -> TInt pos (read $ B.unpack s) }
\" @chars* \"   { \pos s -> mkString pos s }

@typeid  { \pos s -> TTypeId pos s }
@objid   { \pos s -> TObjId pos s }

-- Symbols
":"  { \pos _ -> TColon pos }
";"  { \pos _ -> TSemiColon pos }
"@"  { \pos _ -> TAt pos }
"."  { \pos _ -> TDot pos }
","  { \pos _ -> TComma pos }
"<-" { \pos _ -> TArrow pos }
"+"  { \pos _ -> TPlus pos }
"-"  { \pos _ -> TMinus pos }
"*"  { \pos _ -> TStar pos }
"/"  { \pos _ -> TSlash pos }
"~"  { \pos _ -> TTilde pos }
"<"  { \pos _ -> TLt pos }
"<=" { \pos _ -> TLe pos }
">"  { \pos _ -> TGt pos }
">=" { \pos _ -> TGe pos }
"="  { \pos _ -> TEq pos }
"{"  { \pos _ -> TLBrace pos }
"}"  { \pos _ -> TRBrace pos }
"("  { \pos _ -> TLParen pos }
")"  { \pos _ -> TRParen pos }



{
data Token = TInt AlexPosn Int
           | TString AlexPosn B.ByteString
           | TTypeId AlexPosn B.ByteString
           | TObjId AlexPosn B.ByteString
           | TClass AlexPosn
           | TElse AlexPosn
           | TFalse AlexPosn
           | TFi AlexPosn
           | TIf AlexPosn
           | TIn AlexPosn
           | TInherits AlexPosn
           | TIsvoid AlexPosn
           | TLet AlexPosn
           | TLoop AlexPosn
           | TPool AlexPosn
           | TThen AlexPosn
           | TWhile AlexPosn
           | TCase AlexPosn
           | TEsac AlexPosn
           | TNew AlexPosn
           | TOf AlexPosn
           | TNot AlexPosn
           | TTrue AlexPosn
           | TColon AlexPosn
           | TSemiColon AlexPosn
           | TAt AlexPosn
           | TDot AlexPosn
           | TComma AlexPosn
           | TArrow AlexPosn
           | TPlus AlexPosn
           | TMinus AlexPosn
           | TStar AlexPosn
           | TSlash AlexPosn
           | TTilde AlexPosn
           | TLt AlexPosn
           | TLe AlexPosn
           | TGt AlexPosn
           | TGe AlexPosn
           | TEq AlexPosn
           | TLBrace AlexPosn
           | TRBrace AlexPosn
           | TLParen AlexPosn
           | TRParen AlexPosn
           deriving (Show, Eq)


mkString :: AlexPosn -> B.ByteString -> Token
mkString pos s =
    let s' = B.init (B.tail s) in
    TString pos s'
}
