{
{-# OPTIONS -w  #-}
module Lexer where

import Prelude hiding (lex)

}

%wrapper "monad"

$digit = 0-9
$alpha = [A-Za-z]

tokens :-
  $white+                               ;
  "--".*                                ;
  let                                   { lex' TokenLet         }
  in                                    { lex' TokenIn          }
  $digit+                               { lex (TokenInt . read) }
  $alpha [$alpha $digit \_ \']*         { lex  TokenVar         }
  \=                                    { lex' TokenEq          }
  \+                                    { lex' TokenPlus        }
  \-                                    { lex' TokenMinus       }
  \*                                    { lex' TokenTimes       }
  \/                                    { lex' TokenDiv         }
  \(                                    { lex' TokenLParen      }
  \)                                    { lex' TokenRParen      }
  
{
-- The token type:
data Token
  = TokenLet
  | TokenIn
  | TokenInt Int
  | TokenVar String
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenLParen
  | TokenRParen
  | TokenEOF
  deriving (Eq,Show)

alexEOF = return TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> a) -> AlexAction a
lex f = \(_,_,_,s) i -> return (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: a -> AlexAction a
lex' = lex . const

}
