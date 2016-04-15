----------------------------------------------------------------------
-- |
-- Module       : Data.Transform.Separators
-- Copyright    : (c) 2010 Daniel Fischer
-- Licence      : MIT
--
-- Maintainer  :  Daniel Fischer <daniel.is.fischer@web.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Characters to visually separate words in compound identifiers.
----------------------------------------------------------------------
module Data.Transform.Separators ( -- * Separation Characters
                                   hyphen
                                 , lowLine
                                 , doubleLowLine
                                 , wideLowLine
                                 ) where

-- | Unicode hyphen, U+2010
hyphen          :: Char
hyphen          = '\x2010'

-- | Low line or underscore, U+005F
lowLine         :: Char
lowLine         = '\x5F'

-- | Double low line, U+2017
doubleLowLine   :: Char
doubleLowLine   = '\x2017'

-- | Wide low line, U+FF3F
wideLowLine     :: Char
wideLowLine     = '\xFF3F'
