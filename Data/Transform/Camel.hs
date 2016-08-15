------------------------------------------------------------------------
-- |
-- Module      :  Data.Transform.Camel
-- Copyright   :  (c) 2010 Daniel Fischer
-- Licence     :  MIT
--
-- Maintainer  :  Daniel Fischer <daniel.is.fischer@web.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Transform separated_words identifiers to camelCase in Haskell source.
-- Based on Richard O\'Keefe\'s preprocessor hspp.
------------------------------------------------------------------------
module Data.Transform.Camel (camelSource) where

import Data.Char (isLower, isAlpha, toUpper)
import Data.Transform.Utils
import Prelude

-- | Transform Haskell code written in separated_words style
--   to the more common camelCase style.
--
--   @camelSource sep source@ removes all occurences of @sep@
--   in identifiers in @source@ between two letters of which
--   the first is in lower case after processing and
--   transforms the second to upper case.
--   Thus @camelSource \'_\' \"a_b_c\" == \"aB_c\"@ since after
--   processing the first underscore, the second is no longer
--   preceded by a lowercase letter.
--
--   Comments (and 'String' literals) are not transformed, so
--   haddock comments may need manual fixing.
camelSource :: Char -> String -> String
camelSource = code

code :: Char -> String -> String
code s ('{':'-':cs)             = '{' : '-' : nestedComment (code s) 1 cs
code s ('-':'-':cs)
    | null after                = '-' : '-' : cs
    | isOpChar (head after)     = '-' : '-' : dashes ++ code s after
    | otherwise                 = '-' : '-' : dashes ++ lineComment (code s) after
      where
        (dashes,after) = span (== '-') cs
code s (c:cs)
    | isOpChar c                =  c  : syms ++ code s after
      where
        (syms,after) = span isOpChar cs
code s (c:'\'':cs)
    | isIdPlain c               =  c  : '\'' : ids ++ code s after
      where
        (ids,after) = span isIdChar cs
code s ('"':cs)                 = '"' : string (code s) '"' cs
code s ('\'':cs)                = '\'': string (code s) '\'' cs
code s (a:b:c:cs)
    | removeSep s a b c         =  a  : toUpper c : code s cs
code s (c:cs)                   =  c  : code s cs
code _ _                        =  ""


removeSep :: Char -> Char -> Char -> Char -> Bool
removeSep s a b c = isLower a && b == s && isAlpha c

