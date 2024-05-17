{-# LANGUAGE LambdaCase #-}

module Data.KVITable.Render.Internal where

import qualified Data.List as L
import           Data.Name ( nullName, nameLength, nameText )
import           Data.String ( fromString )
import qualified Data.Text as T

import           Data.KVITable
import           Data.KVITable.Internal.Helpers
import           Data.KVITable.Render


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
      rank e = if (not $ nullName e) &&
                  or [ T.head (nameText e) `elem` ['0'..'9']
                     , T.last (nameText e) `elem` ['0'..'9']
                     ]
               then nameLength e
               else 0
  in snd <$> L.sort skvs


-- TODO: to allow for hideBlankCols, the KVITable should keep track of what the
-- pre-declared vals for each key are v.s. what additional vals may be set by
-- setting a value in that region.  Then there is enough information here
-- (without re-scanning the table) to properly compute the keyvals that should be
-- displayed (which would also mean that the hideCols/hideRows determinations in
-- the rendering functions below are no longer needed.

renderingKeyVals :: RenderConfig -> KeyVals -> KeyVals
renderingKeyVals cfg inpKvs =
  case colStackAt cfg of
    Nothing ->
      -- width is just keys, height is combination of keys and values
      let maxNumKeys = maxCells cfg
          origNumKeys = toEnum $ length kvs
          okKvs = if origNumKeys > maxNumKeys
                  then snoc (take (fromEnum maxNumKeys) kvs)
                       (fromString
                        $ "{+ " <> show (origNumKeys - maxNumKeys) <> " MORE}"
                       , mempty
                       )
                  else kvs
          -- n.b. maxCols is not really useful here, since all but the last
          -- column are headers and values are only shown in that last column.
      in snd $ trimStacked True 1 maxNumKeys okKvs
    Just c ->
      let maxNumCols = min (maxCells cfg) (maxCols cfg)
          (kvsRows, kvsCols) = span ((c /=) . fst) kvs
          numRegularColKvs = let v = length kvs - length kvsCols
                             in if v < 0 then error "BAD1" else toEnum v
          numStackedCols = countStacked kvsCols
          origNumCols = numRegularColKvs + numStackedCols
          allowedNumCols = subOrDef 1 maxNumCols numRegularColKvs
          okKvsCols = if origNumCols > maxNumCols
                      then if numStackedCols <= maxNumCols
                           then kvsCols
                           else snd $ trimStacked False 1 allowedNumCols kvsCols
                      else kvsCols
          allowedNumRows = subOrDef 1 (maxCells cfg)
                           (if origNumCols > maxNumCols
                             then if numStackedCols <= maxNumCols
                                  then numStackedCols
                                  else allowedNumCols
                             else numStackedCols
                           )
          eachRowCols = min maxNumCols numStackedCols
          okKvsRows = snd $ trimStacked False eachRowCols allowedNumRows kvsRows
      in okKvsRows <> okKvsCols

  where

    -- subtracting Naturals must be done carefully to not allow the result to be
    -- < 0; a post-subtraction max is not enough to protect against the initial
    -- value.
    subOrDef d a b = if a < b then d else a - b

    kvs = if sortKeyVals cfg
          then fmap sortWithNums <$> inpKvs
          else inpKvs
    countStacked = \case -- does not allow for hiddenCols
      [] -> 1
      ((_,vs):r) -> toEnum (L.length vs) * countStacked r
    trimStacked _ each n [] = ((n,each), [])
    trimStacked _mulSubs each n ((k,vs):[]) =
      let lvs = toEnum $ length vs
          mvs = foldl (\a b -> if b * each < n then b else a) 1 $ [0..lvs]
          tvs = snoc (take (fromEnum mvs) vs) $ excessHdr $ lvs - mvs
          rvs = if mvs < lvs then tvs else vs
      in ((subOrDef 0 n mvs, mvs * each), [(k,rvs)])
    trimStacked mulSubs each n ((k,vs):rkvs) =
      let lvs = toEnum $ length vs
          ((n',w), kvs') = trimStacked mulSubs each n rkvs
          mvs = foldl (\a b -> if b * w < n then b else a) 1 $ [0..lvs]
          tvs = snoc (take (fromEnum mvs) vs) $ excessHdr $ lvs - mvs
          rvs = if mvs < lvs then tvs else vs
      in ((subOrDef 0 n' mvs, mvs * w), (k,rvs):kvs')
    excessHdr n = fromString $ "{+" <> show n <> "}"
