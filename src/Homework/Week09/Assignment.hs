module Homework.Week09.Assignment (
  zeroOrMore,
  oneOrMore,
  spaces,
  ident,
  parseSExpr,
  Ident(..),
  Atom(..),
  SExpr(..)
) where

import           Control.Applicative
import           Data.Char
import           Homework.Week09.AParser

-- #1
zeroOrMore :: Parser a -> Parser [a]
-- zeroOrMore p = anyP
--   where anyP = someP <|> pure []
--         someP = (fmap (:) p) <*> anyP
zeroOrMore p = oneOrMore p <|> pure []

--breakdown: Parser(p : (p : (p : (p : (someP <|> pure [])  <|> pure [])  <|> pure [])  <|> pure []) )

oneOrMore :: Parser a -> Parser [a]
-- oneOrMore p = someP
--   where anyP = someP <|> pure []
--         someP = (fmap (:) p) <*> anyP

-- oneOrMore p = (:) <$> p <*> zeroOrMore p
oneOrMore p = liftA2 (:) p (zeroOrMore p)


-- #2
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
-- ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)
ident = liftA2 (:) (satisfy isAlpha) (zeroOrMore (satisfy isAlphaNum))

-- #3
type Ident = String

data Atom = N Integer
          | I Ident
  deriving (Eq, Show)

data SExpr = A Atom
           | Comb [SExpr]
  deriving (Eq, Show)

parseSExpr :: Parser SExpr
parseSExpr = stripSpaces
  where stripSpaces = spaces *> p <* spaces
        p = atomP <|> combP
        atomP = intP <|> identP
        intP = A . N <$> posInt
        identP = A . I <$> ident
        combP = Comb <$> sExprP
        sExprP = char '(' *> oneOrMore parseSExpr <* char ')'
