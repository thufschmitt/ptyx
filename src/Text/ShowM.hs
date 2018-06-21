{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Text.ShowM
  ( ShowM(..)
  , fromShow
  ) where

import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Builder as Builder

class Monad m => ShowM m a where
  showMPrec :: Int -> a -> m Builder.Builder
  showM :: a -> m Text.Text

  showMPrec _ x = Builder.fromLazyText <$> showM x
  showM x = Builder.toLazyText <$> showMPrec 0 x

  {-# MINIMAL showM | showMPrec #-}

instance Monad m => ShowM m String where
  showM = pure . Text.pack

fromShow :: (Monad m, Show a) => a -> m Text.Text
fromShow = pure . Text.pack . show
