module Homework.Week12.Ring where

 -- You can optionally include a list of symbols after an import statement
 -- to say exactly what you're importing from the other module. This is sometimes
 -- useful for documentation, and to avoid name clashes between modules.
import Control.Arrow ( first )
import Data.Maybe    ( listToMaybe )
import Data.List     ( stripPrefix )

-- Definition for rings
class Ring a where
  addId  :: a            -- additive identity
  addInv :: a -> a       -- additive inverse
  mulId  :: a            -- multiplicative identity

  add :: a -> a -> a     -- addition
  mul :: a -> a -> a     -- multiplication

-- Something is parsable if there is a way to read it in from a string
class Parsable a where
  -- | If successful, 'parse' returns the thing parsed along with the
  -- "leftover" string, containing all of the input string except what
  -- was parsed
  parse :: String -> Maybe (a, String)

-- | Use the 'parse' method to parse all of a string, requiring that there
-- are no leftovers.
parseAll :: Parsable a => String -> Maybe a
parseAll str = do   -- using the Maybe monad
  (result, "") <- parse str
  return result

instance Parsable Integer where
  parse = listToMaybe . reads
    -- Look up these functions online. Think about why this combination works.
    -- (It should work for any member of the `Read` class. Like `Integer`.)

-- The canonical instance for integers:
instance Ring Integer where
  addId  = 0
  addInv = negate
  mulId  = 1

  add = (+)
  mul = (*)

-- Numbers modulo 5:
data Mod5 = MkMod Integer
  deriving (Show, Eq)

unMod :: Mod5 -> Integer
unMod (MkMod n) = n

mkMod :: Integer -> Mod5
mkMod = MkMod . (`mod` 5)

instance Ring Mod5 where
  addId  = mkMod 0
  addInv = mkMod . negate . unMod
  mulId  = mkMod 1

  add x y = mkMod (unMod x + unMod y)
  mul x y = mkMod (unMod x * unMod y)

instance Parsable Mod5 where
  parse str = do (n, rest) <- parse str
                 return (mkMod n, rest)

-- 2x2 matrices
data Mat2x2 = MkMat Integer Integer Integer Integer
  deriving (Show, Eq)

instance Ring Mat2x2 where
  addId  = MkMat 0 0 0 0
  addInv (MkMat a b c d) = MkMat (negate a) (negate b) (negate c) (negate d)
  mulId  = MkMat 1 0 0 1

  add (MkMat a1 b1 c1 d1) (MkMat a2 b2 c2 d2)
    = MkMat (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

  mul (MkMat a1 b1 c1 d1) (MkMat a2 b2 c2 d2)
    = MkMat (a1 * a2 + b1 * c2)
            (a1 * b2 + b1 * d2)
            (c1 * a2 + d1 * c2)
            (c1 * b2 + d1 * d2)

instance Parsable Mat2x2 where
  parse str = do
    str      <- stripPrefix "[[" str
    (a, str) <- parse str
    str      <- stripPrefix ","  str
    (b, str) <- parse str
    str      <- stripPrefix "][" str
    (c, str) <- parse str
    str      <- stripPrefix ","  str
    (d, str) <- parse str
    str      <- stripPrefix "]]" str
    return (MkMat a b c d, str)

-- Boolean algebra
instance Ring Bool where
  addId  = False
  addInv = not
  mulId  = True

  add = (||)
  mul = (&&)

instance Parsable Bool where
  parse = listToMaybe . reads
