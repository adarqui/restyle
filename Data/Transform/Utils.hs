----------------------------------------------------------------------
--
-- Module       : Data.Transform.Utils
-- Copyright    : (c) 2010 Daniel Fischer
-- Licence      : MIT
--
-- Maintainer  :  Daniel Fischer <daniel.is.fischer@web.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility functions for the transformations.
----------------------------------------------------------------------
module Data.Transform.Utils where

import Data.Char
import Prelude

isOpChar :: Char -> Bool
isOpChar c  = c `notElem` ("(),;[]`{}_'\"" :: String) && (isSymbol c || isPunctuation c)

isIdChar :: Char -> Bool
isIdChar c = c == '\'' || isIdPlain c

isIdPlain :: Char -> Bool
isIdPlain c = c == '_' || isAlphaNum c

nestedComment :: (String -> String) -> Integer -> String -> String
nestedComment cont 0 cs             = cont cs
nestedComment cont l ('-':'}':cs)   = '-' : '}' : nestedComment cont (l-1) cs
nestedComment cont l ('{':'-':cs)   = '{' : '-' : nestedComment cont (l+1) cs
nestedComment cont l (c:cs)         =  c  : nestedComment cont l cs
nestedComment _ _ _                 =  ""

lineComment :: (String -> String) -> String -> String
lineComment cont ('\n':cs)  = '\n' : cont cs
lineComment cont (c:cs)     =  c   : lineComment cont cs
lineComment _ _             = ""

string :: (String -> String) -> Char -> String -> String
string cont del ('\\':c:cs)     = '\\' : c : string cont del cs
string cont del (c:cs)
    | c == del                  =   c  : cont cs
    | otherwise                 =   c  : string cont del cs
string _ _   _                  =  ""
