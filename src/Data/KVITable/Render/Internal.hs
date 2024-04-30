module Data.KVITable.Render.Internal where

import qualified Data.List as L
import qualified Data.Text as T

import           Data.KVITable


-- | Given some KeyVals, sort them if the configuration indicates they should be
-- sorted.

sortedKeyVals :: KeyVals -> Key -> [KeyVal]
sortedKeyVals kmap key = maybe mempty id $ L.lookup key kmap


-- | Sorting for KeyVals.  If the value starts or ends with a digit,
-- then this should do a rough numeric sort on the expectation that
-- the digits represent a version or some other numeric value.  As an
-- approximation of a numeric sort, sort by word size and then string
-- value.  This will result in [ "1", "2", "10", "50", "400" ], but
-- would fail with [ "v1.0", "v2.0", "v3.0", "v2.0.5", "v1.0.0.3" ],
-- but it's a reasonably fast heuristic and probably better than a
-- straight ascii sort.
--
-- This function is used by the 'KVITable' rendering functions.

sortWithNums :: [KeyVal] -> [KeyVal]
sortWithNums kvs =
  let skvs = zip (rank <$> kvs) kvs
      rank e = if (not $ T.null e) &&
                  or [ T.head e `elem` ['0'..'9']
                     , T.last e `elem` ['0'..'9']
                     ]
               then T.length e
               else 0
  in snd <$> L.sort skvs
