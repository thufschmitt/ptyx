{-|
Description: typeclasses for set-theoretic datatypes

Provides set-theoretic connectives and related operations
-}
module Types.SetTheoretic where

-- | Typeclass for types with set-theoretic (unions, intersections, ...)
-- operations
class Ord a => SetTheoretic_ a where

  -- | The empty set
  empty :: a

  -- | The maximal set that contains all the other sets
  full :: a

  -- | Set-theoretic operations
  cup :: a -> a -> a
  cap :: a -> a -> a
  diff :: a -> a -> a
  neg :: a -> a

  -- | Infix version of the operators
  (\/) :: a -> a -> a
  (/\) :: a -> a -> a
  (\\) :: a -> a -> a

  -- | N-ary versions of the set-theoretic operators
  cupN :: [a] -> a
  capN :: [a] -> a

  (\/) = cup
  (/\) = cap
  (\\) = diff

  cupN = foldl cup empty
  capN = foldl cap full

  neg = diff full
  diff x y = x \\ neg y

-- | SetTheoretic with tests for emptyness and containment
--
-- One may be automatically defined in term of the other
class SetTheoretic_ a => SetTheoretic a where
  isEmpty :: a -> Bool
  sub :: a -> a -> Bool

  -- | Infix version of sub
  (<:) :: a -> a -> Bool

  sub x1 x2 = isEmpty $ diff x1 x2
  (<:) = sub
  isEmpty x = sub x empty
