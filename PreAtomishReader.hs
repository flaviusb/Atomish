{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

module PreAtomishReader
(atomish_read) where

import Data.Data
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.List as List
import Text.Peggy 
import Data.Maybe

import PreAtomishGrammar

--data AST = ASTText [Segment] | ASTInteger | ASTDecimal | ASTRegexp | ASTMessage | ASTCurl | ASTSquare | ASTRound | ASTTerminator | ASTAlien 
--data Segment = ASTTextNode String | ASTInterpolateNode AST

atomish_read :: String -> Either String MessageChain
atomish_read text = (case (parseString preatomish "preatomish" text) of
        Right tree -> Right (coalesceStrings (shuffleOperators tree))
        Left _     -> Left "Error")

nopProcess :: MessageChain -> MessageChain
nopProcess x = x

coalesceStrings  = nopProcess
shuffleOperators = nopProcess

-- data Cell = Cell { cells: Map.Map }
-- objectspace
-- readtable
-- bootstrap script: create Ground, create Aliens, create Dark Mirror and Universe, create readtable, create module loader, load standard libs
