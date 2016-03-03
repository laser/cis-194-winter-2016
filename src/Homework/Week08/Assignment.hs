module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase,
  splitWith,safeInt,twobytwo,grouple
) where

import Data.Maybe
import Homework.Week08.AParser
import Control.Applicative
import Data.Char
import Data.Either


-- #1
first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a,c)

instance Functor Parser where
  fmap fn (Parser run)  = Parser f
    where
      f = fmap (first fn) . run

-- #2
instance Applicative Parser  where
  pure  a = Parser $ \s -> Just (a,s)
  (<*>) (Parser fab )  (Parser fa) = Parser $ (\ s -> apply $ fa s  )
       where apply (Just (a,rest)) = fmap (\ (fn,r) -> (fn a, r)) $ fab rest
             apply _  = Nothing

-- #3

twobytwo = go
  where go [] = []
        go (x:y: xs) = [x , y] : go xs
        go (x:xs)    = [x] : go xs

grouple srch xs = let (f,s) = span (== srch) $ snd $ span (/= srch) $  twobytwo xs
                  in concat <$> [f,s]

abParser :: Parser (Char, Char)
abParser = Parser $ f
  where f = (\ s -> cooler $  grouple "ab" s)
        cooler [ (a : b :[] ), rest ] = Just ((a,b), rest)
        cooler _ = Nothing
        f' ('a' : 'b' : xs)  = Just (('a','b'), xs)
        f' _                 = Nothing

abParser_ :: Parser ()
abParser_ = Parser $ f
  where f = (\ s-> cooler $ grouple "ab" s)
        cooler [(_:_:[]), rest ] = Just ((), rest)
        cooler _                 = Nothing

intPair :: Parser [Integer]
intPair = Parser f'
  where
    f'      = parser . span isRight . fmap safeInt . splitWith (==' ' )
    parser ( rightxs, leftxs )
      | length rightxs == 2 = Just (rights rightxs, mconcat $ lefts leftxs)
      | otherwise  = Nothing

safeInt :: String -> Either String Integer
safeInt s = maybeInt
  where allDigits = all isDigit s
        maybeInt
                  | allDigits  = Right (read s :: Integer)
                  | otherwise  = Left s

-- im too lazy to declare dep in cabal so im re-inventing the wheel
splitWith :: (a -> Bool) ->  [a] -> [[a]]
splitWith _  []   = []
splitWith pred xs = filter ( (0  < ) . length) $
                        (takeWhile (not . pred) $ dropWhile pred xs ) :
                           (splitWith pred $ dropWhile (not . pred)$ dropWhile pred xs)

-- #4
instance Alternative Parser where
  empty = undefined
  _ <|> _ = undefined

-- #5
intOrUppercase :: Parser ()
intOrUppercase = undefined
