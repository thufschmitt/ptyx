{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Node
  ( T(..)
  , MemoMonad
  , Memo
  , noId
  , run
  , runEmpty
  , new
  ) where

import           Prelude hiding (id)

import           Control.Applicative (liftA2)
import qualified Control.Monad.State as SM
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Builder as Builder
import qualified Text.ShowM as ShowM
import           Types.SetTheoretic
import qualified Types.UId as UId

data T a = T { typ :: a, id :: Maybe UId.T }
  deriving (Show)

instance Eq a => Eq (T a) where
  T { id = Just id1 } == T { id = Just id2 } = id1 == id2
  T { id = Just _ } == _ = False
  _ == T { id = Just _ } = False
  n1 == n2 = typ n1 == typ n2

instance Ord a => Ord (T a) where
  T { id = Just id1 } `compare` T { id = Just id2 } = id1 `compare` id2
  n1 `compare` n2 = typ n1 `compare` typ n2

instance ShowM.ShowM (SM.State UIdSet) a
  => ShowM.ShowM (SM.State UIdSet) (T a) where
    showMPrec prec T { typ, id = Just id } = do
      existsInSet <- SM.gets (Set.member id)
      if existsInSet
         then pure . Builder.fromString $ ("X" ++ show id)
         else do
           let varName = Builder.fromString $ "X" ++ show id
           body_expr <-
             SM.withState
               (Set.insert id) $
               ShowM.showMPrec prec typ
           pure $ varName <> " where " <> varName <> " = " <> body_expr
    showMPrec prec T { typ, id = Nothing } = ShowM.showMPrec prec typ

-- instance Show a => Show (T a) where show = show . typ

type UIdSet = Set.Set UId.T

type MemoMonad = SM.MonadState UIdSet
type Memo = SM.State UIdSet

run :: UIdSet -> Memo a -> a
run = flip SM.evalState

runEmpty :: Memo a -> a
runEmpty = run Set.empty

instance Functor T where
  fmap f (T x _) = noId $ f x

instance Applicative T where
  pure = flip T Nothing
  liftA2 f (T x _) (T y _) = noId $ f x y

instance Monad T where
  (T a _) >>= f = f a

noId :: a -> T a
noId = flip T Nothing

instance SetTheoretic_ a => SetTheoretic_ (T a) where
  empty = pure empty
  full = pure full
  cup = liftA2 cup
  cap = liftA2 cap
  diff = liftA2 diff
  neg = fmap neg

instance SetTheoretic MemoMonad a => SetTheoretic MemoMonad (T a) where
  isEmpty T { typ, id } = do
      currentMemo <- SM.get
      let isAlreadyProven =
            case id of
              Nothing -> False
              Just x -> Set.member x currentMemo
      if isAlreadyProven
        then pure True
        else do
          case id of
            Nothing -> pure ()
            Just x -> SM.put (Set.insert x currentMemo)
          isEmpty typ

new :: UId.MonadGen m => a -> m (T a)
new elt = do
  uid <- UId.fresh
  pure $ T elt (Just uid)
