module Bdd (Bdd,
            singleton,
            compute,
            empty, full,
            cup, cap, diff,
            (\/), (/\), (\\)
            ) where

data BddT a
    = Leaf Bool
    | Split a (BddT a) (BddT a)
    deriving (Show)

singleton :: a -> BddT a
singleton x = Split x (Leaf True) (Leaf False)

compute :: (a -> Bool) -> BddT a -> Bool
compute _ (Leaf l) = l
compute f (Split x l r)
  | f x = compute f l
  | otherwise = compute f r

class Bdd a where
  empty :: a
  full :: a
  cup :: a -> a -> a
  cap :: a -> a -> a
  diff :: a -> a -> a
  -- Infix version of the operators
  (\/) :: a -> a -> a
  (/\) :: a -> a -> a
  (\\) :: a -> a -> a

  (\/) = cup
  (/\) = cap
  (\\) = diff

instance Ord a => Bdd (BddT a) where
  empty = Leaf False
  full  = Leaf True

  cup (Leaf True) _ = Leaf True
  cup _ (Leaf True) = Leaf True
  cup a (Leaf False) = a
  cup (Leaf False) a = a
  cup b1 b2 =
    let (Split a1 c1 d1) = b1
        (Split a2 c2 d2) = b2
    in
    recurse cup b1 b2 a1 c1 d1 a2 c2 d2

  cap (Leaf False) _ = Leaf False
  cap _ (Leaf False) = Leaf False
  cap a (Leaf True)  = a
  cap (Leaf True) a  = a
  cap b1 b2 =
    let (Split a1 c1 d1) = b1
        (Split a2 c2 d2) = b2
    in
    recurse cap b1 b2 a1 c1 d1 a2 c2 d2

  diff (Leaf False) _ = Leaf False
  diff _ (Leaf True)  = Leaf False
  diff a (Leaf False) = a
  diff (Leaf True) (Split a c d) =
    Split a (diff (Leaf True) c) (diff (Leaf True) d)
  diff b1 b2 =
    let (Split a1 c1 d1) = b1
        (Split a2 c2 d2) = b2
    in
    recurse diff b1 b2 a1 c1 d1 a2 c2 d2

recurse :: Ord a => (t1 -> t -> BddT a) -> t1 -> t -> a -> t1 -> t1 -> a -> t -> t -> BddT a
recurse op b1 b2 a1 c1 d1 a2 c2 d2 =
  case () of _
              | a1 == a2 -> Split a1 (op c1 c2) (op d1 d2)
              | a1 < a2 -> Split a1 (op c1 b2) (op d1 b2)
              | otherwise -> Split a2 (op b1 c2) (op b1 d2)
