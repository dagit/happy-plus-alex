{
{-# OPTIONS -w #-}
module Parser( parseExp ) where

import Language
import Lexer

}

-- The expression language used here comes straight from the happy
-- documentation with virtually no changes (open, so TokenOB/TokenCB were
-- changed to TokenLParen/TokenRParen

%name parse
%tokentype { Token }
%monad { Either String }
%error { errorP }

%token
      let             { Token _ TokenLet }
      in              { Token _ TokenIn }
      int             { Token _ (TokenInt $$) }
      var             { Token _ (TokenVar $$) }
      '='             { Token _ TokenEq }
      '+'             { Token _ TokenPlus }
      '-'             { Token _ TokenMinus }
      '*'             { Token _ TokenTimes }
      '/'             { Token _ TokenDiv }
      '('             { Token _ TokenLParen }
      ')'             { Token _ TokenRParen }

%%

Exp   : let var '=' Exp in Exp  { Let $2 $4 $6 }
      | Exp1                    { Exp1 $1 }

Exp1  : Exp1 '+' Term           { Plus $1 $3 }
      | Exp1 '-' Term           { Minus $1 $3 }
      | Term                    { Term $1 }

Term  : Term '*' Factor         { Times $1 $3 }
      | Term '/' Factor         { Div $1 $3 }
      | Factor                  { Factor $1 }

Factor
      : int                     { Int $1 }
      | var                     { Var $1 }
      | '(' Exp ')'             { Brack $2 }

{
errorP :: [Token] -> Either String a
errorP [] = Left "unexpected end of file"
errorP (Token (AlexPn _ l c) t:_) = Left $
  show l ++ ":" ++
  show c ++ ": unexpected " ++
  unLex t

parseExp :: FilePath -> String -> Either String Exp
parseExp filename contents =
  case parse (alexScanTokens contents) of
    Left e -> Left (filename ++ ":" ++ e)
    Right e -> Right e
}
