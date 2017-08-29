module Bdd where

data BddT a
    = Leaf Bool
    | Split a (BddT a) (BddT a)
    deriving (Eq, Ord, Show)

atom :: a -> BddT a
atom x = Split x (Leaf True) (Leaf False)

data FoldParam src target = FoldParam { fpEmpty :: target
                                        , fpFull :: target
                                        , fpCup :: target -> target -> target
                                        , fpCap :: target -> target -> target
                                        , fpDiff :: target -> target -> target
                                        , fpAtom :: src -> target
                                        }

foldBdd :: FoldParam a b -> BddT a -> b
foldBdd param bdd =
  case bdd of
    Leaf False -> fpEmpty param
    Leaf True -> fpFull param
    Split x p n ->
      let x' = fpAtom param x
          p' = fpCap param x' (foldBdd param p)
          n' = fpDiff param x' (foldBdd param n)
      in
      fpCup param p' n'

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

get :: BddT a -> [([a], [a])]
get a = get_aux a [] [] []

    where
    get_aux a accu pos neg =
      case a of
        (Leaf True) -> (pos, neg):accu
        (Leaf False) -> accu
        (Split x p n) ->
          let accu' = get_aux p accu (x:pos) neg
          in
          get_aux n accu' pos (x:neg)
