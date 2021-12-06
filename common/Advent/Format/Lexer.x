{
module Advent.Format.Lexer where

import Advent.Format.Types
}
%wrapper "posn"

tokens :-

"("     { token_ TOpenGroup             }
")"     { token_ TCloseGroup            }
"%c"    { token_ TAnyChar               }
"%a"    { token_ TAnyLetter             }
"%s"    { token_ TAnyWord               }
"%u"    { token_ TUnsignedInt           }
"%d"    { token_ TSignedInt             }
"%lu"   { token_ TUnsignedInteger       }
"%ld"   { token_ TSignedInteger         }
"*"     { token_ TMany                  }
"+"     { token_ TSome                  }
"&"     { token_ TSepBy                 }
"|"     { token_ TAlt                   }
"!"     { token_ TBang                  }
"@" .   { token (TAt . tail)            }
"%n"    { token_ (TLiteral '\n')        }
"%" .   { token (TLiteral . head . tail)}
.       { token (TLiteral . head)       }

{
type Action = AlexPosn -> String -> (AlexPosn, Token)

token_ :: Token -> Action
token_ x p _ = (p, x)

token :: (String -> Token) -> Action
token f p str = (p, f str)
}
