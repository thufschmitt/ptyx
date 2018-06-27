-- |
-- A (simple and naive) memoisation monad used to walk through infinite types
-- without looping

module Control.Monad.Memo
  ( T
  , UIdSet
  , run, runEmpty
  ) where

import qualified Control.Monad.State as SM
import qualified Data.Set as Set
import qualified Types.UId as UId

type UIdSet = Set.Set UId.T

type T = SM.State UIdSet

run :: UIdSet -> T a -> a
run = flip SM.evalState

runEmpty :: T a -> a
runEmpty = run Set.empty
