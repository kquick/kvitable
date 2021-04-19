{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.KVITable
  (
    KVITable(KVITable)
  , Key
  , KeyVal
  , fromList
  , toList
  )
where

import           Data.Function ( on )
import qualified Data.Map as Map
import           Data.Text ( Text )
import qualified GHC.Exts


-- | The core KeyValue Indexed Table.  This table is similar to a Map,
-- but the values are indexed by a list of Key+Value combinations, and
-- the table contents can be sparse.

data KVITable v = KVITable { contents     :: Map.Map [(Key, KeyVal)] v
                           , valuecolName :: Text  -- ^ name of the value cells
                           }

instance Eq v => Eq (KVITable v) where
  (==) = (==) `on` contents

instance Show v => Show (KVITable v) where
  show t = "KVITable { keyvals = " <> show (contents t) <>
           ", valuecolName = " <> show (valuecolName t) <>
           "}"

type Key = Text
type KeyVal = Text

-- | The KVITable semigroup is left biased (same as Data.Map).
instance Semigroup (KVITable v) where
  a <> b = KVITable { contents = contents a <> contents b
                    , valuecolName = valuecolName a
                    }

instance Monoid (KVITable v) where
  mempty = KVITable { contents = mempty
                    , valuecolName = "Value"
                    }

instance Functor KVITable where
  fmap f t = KVITable { contents = fmap f (contents t)
                      , valuecolName = valuecolName t
                      }

instance Foldable KVITable where
  foldMap f = foldMap f . contents

instance Traversable KVITable where
  traverse f t = (\c -> KVITable { contents = c
                                 , valuecolName = valuecolName t
                                 }
                 ) <$> traverse f (contents t)

instance GHC.Exts.IsList (KVITable v) where
  type Item (KVITable v) = ([(Key, KeyVal)], v)
  fromList = (\c -> mempty { contents = c }) . GHC.Exts.fromList
  toList = GHC.Exts.toList . contents


fromList :: [ GHC.Exts.Item (KVITable v) ] -> KVITable v
fromList = GHC.Exts.fromList

toList :: KVITable v -> [ GHC.Exts.Item (KVITable v) ]
toList = GHC.Exts.toList
