{
module Lexer (
               Token(..)
             , TokenClass(..)
             , tkInt
             , tkString
             , tkObjId
             , tkTypeId
             , isEof
             , alexMonadScan
             , runAlex
             , scan
             , getTokens
             )
where

import Control.Monad (liftM)
import Debug.Trace

}

%wrapper "monadUserState"


$digit = 0-9
$alpha = [a-zA-Z_]
$alphanum = [$alpha  $digit]
$lower = [a-z_]
$upper = A-Z
$white = [\ \t\r\n\v\f]

@integer = 0 | [1-9][0-9]*
@typeid = $upper $alphanum*
@objid  = $lower $alphanum*
--@chars = [^ \" \0 \\] | \\[^ \0] | \\\n
@chars = [^\"] | \\\n
@any = . | \n

tokens :-

<0> $white+      { skip }
<0> "--".*\n     { skip }
<0>       "(*"   { increaseDepth `andBegin2` comment }
<comment> "(*"   { increaseDepth }
<comment> "*)"   { decreaseDepth }
<comment> @any   { skip }

-- Keywords
<0> case     { mkTk TCase }
<0> class    { mkTk TClass }
<0> else     { mkTk TElse }
<0> esac     { mkTk TEsac }
<0> false    { mkTk TFalse }
<0> fi       { mkTk TFi }
<0> if       { mkTk TIf }
<0> in       { mkTk TIn }
<0> inherits { mkTk TInherits }
<0> isvoid   { mkTk TIsvoid }
<0> let      { mkTk TLet }
<0> loop     { mkTk TLoop }
<0> new      { mkTk TNew }
<0> not      { mkTk TNot }
<0> of       { mkTk TOf }
<0> pool     { mkTk TPool }
<0> then     { mkTk TThen }
<0> true     { mkTk TTrue }
<0> while    { mkTk TWhile }


-- Literals
<0> @integer        { \inp@(_, _, s) i -> mkTk (TInt (read $ take i s)) inp i }
<0> \"              { initString `andBegin2` string }
<string> \"         { finishString `andBegin2` 0 }
<string> @chars     { addChar }

<0> @typeid  { \inp@(_, _, s) i -> mkTk (TTypeId $ take i s) inp i }
<0> @objid   { \inp@(_, _, s) i -> mkTk (TObjId $ take i s) inp i }

-- Symbols
<0> ":"  { mkTk TColon }
<0> ";"  { mkTk TSemiColon }
<0> "@"  { mkTk TAt }
<0> "."  { mkTk TDot }
<0> ","  { mkTk TComma }
<0> "<-" { mkTk TAssign }
<0> "=>" { mkTk TFatArrow }
<0> "+"  { mkTk TPlus }
<0> "-"  { mkTk TMinus }
<0> "*"  { mkTk TStar }
<0> "/"  { mkTk TSlash }
<0> "~"  { mkTk TTilde }
<0> "<"  { mkTk TLt }
<0> "<=" { mkTk TLe }
<0> ">"  { mkTk TGt }
<0> ">=" { mkTk TGe }
<0> "="  { mkTk TEq }
<0> "{"  { mkTk TLBrace }
<0> "}"  { mkTk TRBrace }
<0> "("  { mkTk TLParen }
<0> ")"  { mkTk TRParen }



{
--mkTk :: TokenClass -> AlexInput -> Int -> Alex Token
mkTk cls (pos, _, _) _ = return $ MkToken pos cls

--mkString :: A -> Int -> Alex Token
mkString inp s = undefined

data AlexUserState = AlexUserState {
                     commentDepth :: Int
                   , stringValue :: String
                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
                    commentDepth = 0
                  , stringValue = ""
                  }

alexEOF :: Alex Token
alexEOF = return $ MkToken (AlexPn 0 0 0) TEof

data Token = MkToken { tkPos :: AlexPosn, tkClass :: TokenClass }
    deriving (Eq)

instance Show Token where
    show (MkToken (AlexPn _ l c) cl) =
        concat ["<", show cl, " ", show (l, c), ">"]

data TokenClass = TInt { tcInt :: Int }
                | TString { tcString :: String }
                | TTypeId { tcTypeId :: String }
                | TObjId { tcObjId :: String }
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
                | TEof
           deriving (Show, Eq)


tkInt :: Token -> Int
tkInt = tcInt . tkClass

tkString, tkObjId, tkTypeId :: Token -> String
tkString = tcString . tkClass
tkObjId = tcObjId . tkClass
tkTypeId = tcTypeId . tkClass

isEof :: Token -> Bool
isEof (MkToken _ TEof) = True
isEof _                = False

get :: Alex AlexUserState
get = Alex $ \s   -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put u = Alex $ \s -> Right (s{alex_ust=u}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = do ust <- get
              put (f ust)

getCommentDepth = liftM commentDepth get
getStringValue  = liftM stringValue get

incr, decr :: AlexUserState -> AlexUserState
incr state = state { commentDepth = commentDepth state + 1 }
decr state = state { commentDepth = commentDepth state - 1 }

increaseDepth inp len = do
  modify incr
  skip inp len

decreaseDepth inp len = do
  modify decr
  depth <- getCommentDepth
  if depth == 0
     then begin 0 inp len
     else skip inp len

initString inp len = do
  modify (\state -> state { stringValue = "" })
  skip inp len

addChar inp@(_, _, s) len = do
  modify (\state -> state { stringValue = take len s ++ stringValue state })
  skip inp len

finishString inp@(pos, _, _) len = do
  s <- get
  return $ MkToken pos (TString (reverse $ stringValue s))



scan :: String -> Either String [Token]
scan str = runAlex str $ do
  let loop = do tok <- alexMonadScan
                if isEof tok
                    then do code <- alexGetStartCode
                            case code of
                                 0 -> return [tok]
                                 x | x == comment -> error "unterminated comment"
                                   | x == string  -> error "unterminated string"
                    else do toks <- loop
                            return (tok:toks)
  loop

getTokens :: String -> [Token]
getTokens src = case scan src of
                  Left err -> error err
                  Right toks -> toks


(act `andBegin2` code) input len = do alexSetStartCode code; act input len

}
