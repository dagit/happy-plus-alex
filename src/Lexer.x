{
{-# OPTIONS -w  #-}
module Lexer
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , unLex
  , alexScanTokens
  ) where

}

%wrapper "posn"

$digit = 0-9
$alpha = [A-Za-z]

tokens :-
  $white+                               ;
  "--".*                                ;
  let                                   { tok' TokenLet         }
  in                                    { tok' TokenIn          }
  $digit+                               { tok (TokenInt . read) }
  $alpha [$alpha $digit \_ \']*         { tok  TokenVar         }
  \=                                    { tok' TokenEq          }
  \+                                    { tok' TokenPlus        }
  \-                                    { tok' TokenMinus       }
  \*                                    { tok' TokenTimes       }
  \/                                    { tok' TokenDiv         }
  \(                                    { tok' TokenLParen      }
  \)                                    { tok' TokenRParen      }
  .                                     { tok  TokenLexError    }

{

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving ( Show )

data TokenClass
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
  | TokenLexError String
  deriving ( Show )

-- For nice parser error messages.
unLex :: TokenClass -> String
unLex TokenLet = "let"
unLex TokenIn = "in"
unLex (TokenInt i) = show i
unLex (TokenVar s) = show s
unLex TokenEq = "="
unLex TokenPlus = "+"
unLex TokenMinus = "-"
unLex TokenTimes = "*"
unLex TokenDiv = "/"
unLex TokenLParen = "("
unLex TokenRParen = ")"
unLex (TokenLexError c) = "lexical error at character " ++ show c

type Action r = AlexPosn -> String -> r

tok :: (String -> TokenClass) -> Action Token
tok f p str = Token p (f str)

-- For constructing tokens that do not depend on the input
tok' :: TokenClass -> Action Token
tok' = tok . const

}
