{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
 - Description: The type of booleans
 -
 - A subtype of 'Bool' is one of 'True', 'False', 'bottom' or 'Bool'
-}

module Types.Bool (
  T(..)
  ) where

import Types.SetTheoretic


data T = TrueT
          | FalseT
          | Full
          | Empty
          deriving (Eq, Ord)

instance Show T where
  show TrueT = "true"
  show FalseT = "false"
  show Full = "Bool"
  show Empty = "⊥"

cupB :: T -> T -> T
cupB Full _ = Full
cupB _ Full = Full
cupB Empty a = a
cupB a Empty = a
cupB TrueT FalseT = Full
cupB FalseT TrueT = Full
cupB TrueT _ = TrueT
cupB _ FalseT = FalseT

negB :: T -> T
negB Full = Empty
negB Empty = Full
negB TrueT = FalseT
negB FalseT = TrueT

capB :: T -> T -> T
capB b1 b2 = neg $ cupB (neg b1) (neg b2)

subB :: T -> T -> Bool
subB _ Full = True
subB Empty _ = True
subB _ Empty = False
subB Full _ = False
subB x y = x == y

instance SetTheoretic_ T where
  empty = Empty
  full = Full
  cup = cupB
  cap = capB
  neg = negB
  diff x y = x /\ neg y

instance Monad m => SetTheoretic m T where
  isEmpty = pure . (==) Empty
  sub x y = pure $ x `subB` y
