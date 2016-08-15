-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Transform.UnCamel
-- Copyright   :  (c) 2010 Daniel Fischer
-- Licence     :  MIT
--
-- Maintainer  :  Daniel Fischer <daniel.is.fischer@web.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Transform camelCased identifiers to separated_words.
-----------------------------------------------------------------------------
module Data.Transform.UnCamel ( -- * Transformation Functions
                                unCamelHTML
                              , unCamelSource
                              ) where

import Data.Char (isLower, isUpper, toLower)
import Data.Transform.Utils
import Prelude

-- | Transform identifiers in (haddock-produced) HTML files from
--   camelCase to separated_words.
--
--   The separation character is freely choosable, but it is
--   recommended to take one of those in "Transform.Separators". Since
--   underscore-separated identifiers are used in some
--   libraries, choosing 'lowLine' may lead to confusion.
--
--   The separation character is inserted between a lowercase
--   character and an uppercase character immediately following.
--   If that uppercase character is followed by a lowercase letter,
--   it is also transformed to lower case.
--
--   'String' literals appearing in haddock comments are also
--   transformed. Deal with it or yell.
unCamelHTML :: Char -> String -> String
unCamelHTML r ('<':cs)      = '<' : skipTag r cs
unCamelHTML r (a:b:c:cs)
    | hump a b c            =  a  : r : toLower b : unCamelHTML r (c:cs)
unCamelHTML r (a:bs@(b:cs))
    | step a b              =  a  : r : b : unCamelHTML r cs
    | otherwise             =  a  : unCamelHTML r bs
unCamelHTML _ cs            =  cs

hump :: Char -> Char -> Char -> Bool
hump a b c  = isLower a && isUpper b && isLower c

step :: Char -> Char -> Bool
step a b    = isLower a && isUpper b

skipTag :: Char -> String -> String
skipTag r ('>':cs)  = '>' : unCamelHTML r cs
skipTag r (a:cs)    =  a  : skipTag     r cs
skipTag _ ""        =  ""

-- | Transform identifiers in (non-literate) source files from
--   camelCase to separated_words.
--
--   The separation character is freely choosable, but it is
--   recommended to take one of those in "Transform.Separators". Since
--   underscore-separated identifiers are used in some
--   libraries, choosing 'lowLine' may lead to confusion.
--   On the other hand, it is the only one which has a
--   fighting chance of producing valid Haskell code.
--
--   The separation character is inserted between a lowercase
--   character and an uppercase character immediately following.
--   If that uppercase charcter is followed by a lowercase letter,
--   it is also transformed to lower case.
--
--   Operators including two or more consecutive dashes are
--   handled correctly, i.e. @|--@ or @--:@ are not treated
--   as the start of a line-comment.
--
--   Single quotes in identifiers, as in @foldl'@ or @f'2''d@, are
--   not considered to begin a character literal. An unfortunate
--   consequence of that and the simple algorithm is that in an
--   expression like
--
-- >              replicate 5'\\'
--
--   (with no space between number and character literal), the
--   closing quote is considered to begin a character literal.
--
--   Comments are not transformed, which may lead to inconsistencies
--   between code and comments. That may change in the future.
unCamelSource :: Char -> String -> String
unCamelSource = code

code :: Char -> String -> String
code r ('{':'-':cs)             = '{' : '-' : nestedComment (code r) 1 cs
code r ('-':'-':cs)
    | null after                = '-' : '-' : cs
    | isOpChar (head after)     = '-' : '-' : dashes ++ code r after
    | otherwise                 = '-' : '-' : dashes ++ lineComment (code r) after
      where
        (dashes,after) = span (== '-') cs
code r (c:cs)
    | isOpChar c                =  c  : syms ++ code r after
      where
        (syms,after) = span isOpChar cs
code r (c:'\'':cs)
    | isIdPlain c               =  c  : '\'' : ids ++ code r after
      where
        (ids,after) = span isIdChar cs
code r ('"':cs)                 = '"' : string (code r) '"' cs
code r ('\'':cs)                = '\'': string (code r) '\'' cs
code r (a:b:c:cs)
    | hump a b c                =  a  : r : toLower b : code r (c:cs)
code r (a:bs@(b:cs))
    | step a b    =  a  : r : b : code r cs
    | otherwise                 =  a  : code r bs
code _ cs                       =  cs
