{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

module PreAtomishGrammar
(preatomish, MessageChain(..), Expression(..), BracketType(..), Literal(..), LitS(..), Chunk(..)) where
import Data.Data
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.List as List
import Text.Peggy 
import Data.Maybe

-- This is only used to make the PreAtomishReader easier
-- The PreAtomishReader does not understand read macros or MMOPery or syntax or layout

data MessageChain = MessageChain [Expression] deriving (Show, Data, Eq, Typeable)
data Expression = Message String [MessageChain] | Brackets BracketType [MessageChain] | ELiteral Literal | Terminator deriving (Show, Data, Eq, Typeable)
data BracketType = RoundBracket | SquareBracket | CurlyBracket deriving (Show, Data, Eq, Typeable)
data Literal = LitText LitS | LitInteger Int | LitDecimal Double | LitRegexp String deriving (Show, Data, Eq, Typeable)
data LitS = SquareString [Chunk] | QuotedString [Chunk] deriving (Show, Data, Eq, Typeable)
data Chunk = Lit String | Escape String | Interpolate MessageChain | RawInsert String deriving (Show, Data, Eq, Typeable)

[peggy|

preatomish ::: MessageChain
  = messagechain

messagechain ::: MessageChain
  = expression+ { MessageChain $1 }

expression ::: Expression
  = ( message / brackets / literal / terminator )

brackets :: Expression
  = ( ('(' commated ')' { Brackets RoundBracket $1 }) / ('[' commated ']' { Brackets SquareBracket $1 }) / ('{' commated '}' { Brackets CurlyBracket $1 }) )

message :: Expression
  = (name / operator) ('(' commated? ')')? { Message $1 (fromMaybe [] (fromMaybe Nothing $2)) }

name :: String
  = [a-zA-Z] [a-zA-Z_:0-9?]* { $1:$2 }

operator :: String
  = [~!@$%^&*_+-='`/?]*

literal :: Expression
  = ( decimal / number / text ) { ELiteral $1 }

decimal :: Literal
  = [+-]? [0-9]+ ( '.' [0-9]+ )? { LitDecimal (read ((maybeToList $1) ++ $2 ++ (fromMaybe [] $3))) }

number :: Literal
  = [+-]? [0-9]+ { LitInteger (read ((maybeToList $1) ++ $2)) }

text :: Literal
  = (squarestring / quotedstring)

squarestring :: Literal
  = '#[' ( squarelit / squareescape / interpolate )* ']' { LitText (SquareString $1) }

quotedstring :: Literal
  = '\"' ( quotedlit / quotedescape / interpolate )* '\"' { LitText (QuotedString $1) }

squarelit :: Chunk
  = [^\\\]]+ { (Lit $1) }

quotedlit :: Chunk
  = [^\\"]+  { (Lit $1) }  --"

squareescape :: Chunk
  = '\\' [nr\]{] { (Escape ['\\', $1]) }

quotedescape :: Chunk
  = '\\' [nr"{] { (Escape ['\\', $1]) } --"

interpolate :: Chunk
  = '{' messagechain '}' { (Interpolate $1) } -- Nothing here at the moment

terminator :: Expression
  = ('.' / '\n' / '\r') { Terminator }

commated ::: [MessageChain]
  =  messagechain ',' messagechain* { $1:$2 }

|]
