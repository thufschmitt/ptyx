module Types.SetTheoretic where

class Ord a => SetTheoretic_ a where
  empty :: a
  full :: a
  cup :: a -> a -> a
  cap :: a -> a -> a
  diff :: a -> a -> a
  neg :: a -> a

  -- Infix version of the operators
  (\/) :: a -> a -> a
  (/\) :: a -> a -> a
  (\\) :: a -> a -> a

  -- N-ary versions
  cupN :: [a] -> a
  capN :: [a] -> a

  (\/) = cup
  (/\) = cap
  (\\) = diff

  cupN = foldl cup empty
  capN = foldl cap full

  neg = diff full
  diff x y = x \\ neg y

class SetTheoretic_ a => SetTheoretic a where
  isEmpty :: a -> Bool
  sub :: a -> a -> Bool

  -- Infix version of sub
  (<:) :: a -> a -> Bool

  sub x1 x2 = isEmpty $ diff x1 x2
  (<:) = sub
  isEmpty x = sub x empty
