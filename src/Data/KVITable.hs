{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.KVITable
  (
    KVITable(KVITable)
  , Key
  , KeyVal
  , fromList
  , toList
  , Data.KVITable.lookup
  , keyVals
  , valueColName
  , insert
  , rows
  )
where

import           Data.Function ( on )
import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Text ( Text )
import qualified Data.Text as T
import qualified GHC.Exts
import           Lens.Micro ( Lens' )


-- | The core KeyValue Indexed Table.  This table is similar to a Map,
-- but the values are indexed by a list of Key+Value combinations, and
-- the table contents can be sparse.

data KVITable v = KVITable { keyvals      :: KeyVals -- ^ allowed value for keys (in order)
                           , contents     :: Map.Map KeySpec v
                             -- The invariant for the KVITable is that
                             -- each KeySpec contains all keys listed
                             -- in keyvals (in the same order) with
                             -- the defaultKeyVal for any keys not
                             -- explicitly provided for that value.
                           , valuecolName :: Text  -- ^ name of the value cells
                           }

instance Eq v => Eq (KVITable v) where
  -- n.b. keyvals (i.e. metadata) are _not_ used for equality, only contents
  (==) = (==) `on` contents

instance Show v => Show (KVITable v) where
  show t = "KVITable {" <>
           " keyvals = " <> show (keyvals t) <>
           " contents = " <> show (contents t) <>
           ", valuecolName = " <> show (valuecolName t) <>
           "}"

type Key = Text
type KeyVal = Text
type KeySpec = [ (Key,  KeyVal ) ]
type KeyVals = [ (Key, [KeyVal]) ]

defaultKeyVal :: KeyVal
defaultKeyVal = ""

-- | The KVITable semigroup is left biased (same as Data.Map).  Note
-- that joining tables can result in a table that has a different
-- keyVals sequence than either input table.

instance Semigroup (KVITable v) where
  a <> b = foldr (\(ks,v) t -> insert ks v t)
           (mempty { valuecolName = valuecolName a
                   , keyvals = keyvals a
                   })
           (toList b <> toList a)

instance Monoid (KVITable v) where
  mempty = KVITable { keyvals = mempty
                    , contents = mempty
                    , valuecolName = "Value"
                    }

instance Functor KVITable where
  fmap f t = KVITable { contents = fmap f (contents t)
                      , keyvals = keyvals t
                      , valuecolName = valuecolName t
                      }

instance Foldable KVITable where
  foldMap f = foldMap f . contents

instance Traversable KVITable where
  traverse f t = (\c -> KVITable { contents = c
                                 , valuecolName = valuecolName t
                                 , keyvals = keyvals t
                                 }
                 ) <$> traverse f (contents t)

instance GHC.Exts.IsList (KVITable v) where
  type Item (KVITable v) = (KeySpec, v)
  fromList = foldr (\(ks,v) t -> insert ks v t) mempty
  toList = GHC.Exts.toList . contents

fromList :: [ GHC.Exts.Item (KVITable v) ] -> KVITable v
fromList = GHC.Exts.fromList

toList :: KVITable v -> [ GHC.Exts.Item (KVITable v) ]
toList = GHC.Exts.toList


-- | Fetch or set the keyvals list via lenses. Note that setting the
-- keyval list will drop any current contents in the table that do not
-- have entries in the keyvals list.

keyVals :: Lens' (KVITable v) KeyVals
keyVals f t = (\kvs ->
                 t { keyvals = fmap L.sort <$> kvs
                   , contents =
                     let inKVS spec _ = inkv spec kvs
                         inkv [] [] = True
                         inkv ((sk,sv):srs) ((k,vs):kv)
                           | sk == k && sv `elem` vs = inkv srs kv
                         inkv _ _ = False
                     in Map.filterWithKey inKVS (contents t)
                   }
              ) <$> f (keyvals t)


-- | Fetch or set the column name for the actual value cell in the
-- KVITable.

valueColName :: Lens' (KVITable v) Text
valueColName f t = (\n -> t { valuecolName = n } ) <$> f (valuecolName t)


-- | Retrieve an entry from the KVITable given a keyspec.  The keyspec
-- may be minimally specified (i.e. it does not need to contain keys
-- whose value is the default key value) and it may present the keys
-- out of order and the lookup will still succeed (if there is a value
-- for the normalized keyspec), but it will be faster to use the
-- normalized key directly.

lookup :: KeySpec -> KVITable v -> Maybe v
lookup keyspec t = case Map.lookup keyspec $ contents t of
                     Just v -> Just v
                     Nothing ->
                       -- keyspec might be under-specified or in a different order
                       let ks = foldl keyandval [] (keyvals t)
                           keyandval s (k,vs) = case L.lookup k keyspec of
                             Just v -> if v `elem` vs
                                       then s <> [(k,v)]
                                       else [("Lookup key " <>
                                               k <> " value of " <> v <>
                                               " not in known values for that key: "
                                             , T.pack $ show vs)]  -- bogus to fail the lookup below
                             Nothing -> s <> [(k, defaultKeyVal)]
                       in Map.lookup ks $ contents t

-- | Inserts a new cell value into the table at the specified keyspec
-- location.  The keyspec may be minimally specified and out-of-order.
--
-- This may be an expensive operation if it has to extend the keyvals
-- for the table.  In general, insertion is expected to be less
-- frequent than lookups so computation costs are biased towards the
-- insertion operation.

insert :: KeySpec -> v -> KVITable v -> KVITable v
insert keyspec val t =
  let remainingKeyValDefaults = fmap (\(k,_) -> (k, defaultKeyVal))
      addDefVal e@(k,vs) = if defaultKeyVal `elem` vs
                           then e
                           else (k, L.sort $ defaultKeyVal : vs)
      -- endset :: KeyVals -> KeySpec -> KeySpec -> KeyVals -> KVITable v
      endset rkv [] tspec kvbld =
        -- Reached the end of the user's keyspec but there are more
        -- known keyvals in this KVITable, so add the entry with the
        -- default KeyVal for the remaining keyspec (and ensure the
        -- default KeyVal is listed in the table's keyvals).
        let spec = tspec <> remainingKeyValDefaults rkv
        in t { contents = Map.insert spec val (contents t)
             , keyvals = kvbld <> (addDefVal <$> rkv)
             }
      endset [] spec tspec kvbld =
        -- Reached the end of the known keyvals for this table but the
        -- user's keyspec has additional elements.  This should extend
        -- the tables keyvals with the remaining keyspec; also all
        -- existing table values should be pushed out to use the
        -- default values for the new keys in their keyspec.
        let spec' = tspec <> spec
            keyvals' = kvbld <> (fmap (L.sort .
                                       if null curTblList
                                       then (:[])
                                       else (:[defaultKeyVal])) <$> spec)
            curTblList = Map.toList $ contents t
            updTblList = fmap (\(ks,v) -> (ks <> remainingKeyValDefaults spec, v)) curTblList
        in t { contents = Map.insert spec' val $ Map.fromList updTblList
             , keyvals = keyvals'
             }
      endset kvs@((k,vs):rkvs) ((sk,sv):srs) tspec kvbld =
        if k == sk
        then let kv' = if sv `elem` vs
                       then kvbld <> [(k,vs)]
                       else kvbld <> [(k, L.sort $ sv : vs)]
             in endset rkvs srs (tspec <> [(k,sv)]) kv'
        else
          -- re-arrange user spec crudely by throwing invalid
          -- candidates to the end and retrying.  This isn't
          -- necessarily efficient, but keyspecs aren't expected to be
          -- longer than about a dozen entries.
          if sk `elem` (fst <$> rkvs) && k `elem` (fst <$> srs)
          then endset kvs (srs <> [(sk,sv)]) tspec kvbld
          else
            if any (`elem` (fst <$> kvs)) (fst <$> srs)
            then endset kvs (srs <> [(sk,sv)]) tspec kvbld
            else
              let vs' = if defaultKeyVal `elem` vs then vs else (L.sort $ defaultKeyVal : vs)
              in endset rkvs ((sk,sv):srs) (tspec <> [(k,defaultKeyVal)]) (kvbld <> [(k,vs')])
  in endset (keyvals t) keyspec [] []


-- | The 'rows' function returns a set of rows for the KVITable as a
-- list structure, where each list entry is a different row.  A row
-- consists of the /values/ of the keys for that row followed by the
-- value of the entry (to get the names of the keys, use 'keyVals').

rows :: KVITable v -> [ ([KeyVal], v) ]
rows t = go (keyvals t) []
  where
    go [] spec = let spec' = reverse spec
                 in case Map.lookup spec' (contents t) of
                      Nothing -> []
                      Just v -> [ (snd <$> spec', v) ]
    go ((key, vals):kvs) spec =
      concatMap (\v -> let spec' = (key,v):spec in go kvs spec') vals
