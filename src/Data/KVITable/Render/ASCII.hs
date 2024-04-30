{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides the 'KVITable' 'render' function for
-- rendering the table in a plain ASCII format.

module Data.KVITable.Render.ASCII
  (
    render
    -- re-export Render definitions to save the caller an additional import
  , RenderConfig(..)
  , defaultRenderConfig
  )
where

import qualified Data.List as L
import           Data.Maybe ( isNothing )
import           Data.Text ( Text )
import qualified Data.Text as T
import           Lens.Micro ( (^.) )
import qualified Prettyprinter as PP

import           Data.KVITable ( KVITable, KeySpec, Key, keyVals )
import qualified Data.KVITable as KVIT
import           Data.KVITable.Internal.Helpers
import           Data.KVITable.Render
import           Data.KVITable.Render.Internal

import           Prelude hiding ( lookup )


-- | Renders the specified table in ASCII format, using the specified
-- 'RenderConfig' controls.

render :: PP.Pretty v => RenderConfig -> KVITable v -> Text
render cfg t =
  let kseq = fst <$> t ^. keyVals
      (fmt, hdr) = renderHdrs cfg t kseq
      bdy = renderSeq cfg fmt kseq t
  in T.unlines $ hdr <> bdy

----------------------------------------------------------------------

data FmtLine = FmtLine [Int] Sigils Sigils  -- last is for sepline
data Sigils = Sigils { sep :: Text, pad :: Text, cap :: Text }

fmtLine :: [Int] -> FmtLine
fmtLine cols = FmtLine cols
               Sigils { sep = "|", pad = " ", cap = "_" }
               Sigils { sep = "+", pad = "-", cap = "_" }

fmtColCnt :: FmtLine -> Int
fmtColCnt (FmtLine cols _ _) = length cols

perColOvhd :: Int
perColOvhd = 2 -- pad chars on either side of each column's entry

-- | Formatted width of output, including pad on either side of each
-- column's value (but not the outer set), and a separator between columns.
--
-- Note that a column size of 0 indicates that hideBlankCols is active
-- and the column was found to be empty of values, so it should not be
-- counted.
fmtWidth :: FmtLine -> Int
fmtWidth (FmtLine cols _ _) =
  let cols' = L.filter (/= 0) cols
  in sum cols' + ((perColOvhd + 1) * (length cols' - 1))

fmtEmptyCols :: FmtLine -> Bool
fmtEmptyCols (FmtLine cols _ _) = sum cols == 0

fmtAddColLeft :: Int -> FmtLine -> FmtLine
fmtAddColLeft leftCol (FmtLine cols s s') = FmtLine (leftCol : cols) s s'

data FmtVal = Separator | TxtVal Text | CenterVal Text

fmtRender :: FmtLine -> [FmtVal] -> Text
fmtRender (FmtLine _cols _sigils _sepsigils) [] = ""
fmtRender (FmtLine cols sigils sepsigils) vals@(val:_) =
  if length cols == length vals
  then let sig f o = case o of
                       Separator   -> f sepsigils
                       TxtVal _    -> f sigils
                       CenterVal _ -> f sigils
           l = sig sep val
       in l <>
          T.concat
          [ sig pad fld <>
            (case fld of
                Separator   -> T.pack (replicate sz '-')
                TxtVal v    -> T.pack (replicate (sz - T.length v) ' ') <> v
                CenterVal t -> let (w,e) = (sz - T.length t - 2) `divMod` 2
                                   m = cap sigils
                                   ls = T.replicate (w + 0) m
                                   rs = T.replicate (w + e) m
                               in if T.length t + 2 >= sz
                                  then (T.replicate (sz - T.length t) " ") <> t
                                  else ls <> " " <> t <> " " <> rs
            ) <>
            sig pad fld <>
            sig sep fld  -- KWQ or if next fld is Nothing
          | (sz,fld) <- zip cols vals, sz /= 0
          ]
  else error ("Insufficient arguments (" <>
              show (length vals) <> ")" <>
              " for FmtLine " <> show (length cols))

----------------------------------------------------------------------

data HeaderLine = HdrLine FmtLine HdrVals Trailer
type HdrVals = [FmtVal]
type Trailer = Text

hdrFmt :: HeaderLine -> FmtLine
hdrFmt (HdrLine fmt _ _) = fmt

renderHdrs :: PP.Pretty v => RenderConfig -> KVITable v -> Keys -> (FmtLine, [Text])
renderHdrs cfg t keys =
  ( lastFmt
  , [ fmtRender fmt hdrvals
      <> (if T.null trailer then "" else (" <- " <> trailer))
    | (HdrLine fmt hdrvals trailer) <- hrows
    ] <>
    (single $ fmtRender lastFmt (replicate (fmtColCnt lastFmt) Separator)) )
  where
    hrows = hdrstep cfg t keys
    lastFmt = case reverse hrows of
                [] -> fmtLine mempty
                (hrow:_) -> hdrFmt hrow

hdrstep :: PP.Pretty v => RenderConfig -> KVITable v -> Keys -> [HeaderLine]
hdrstep _cfg t [] =
  -- colStackAt wasn't recognized, so devolve into a non-colstack table
  let valcoltxt = t ^. KVIT.valueColName
      valcoltsz = T.length valcoltxt
      valsizes  = length . show . PP.pretty . snd <$> KVIT.toList t
      valwidth  = maxOf 0 $ valcoltsz : valsizes
  in single $ HdrLine (fmtLine $ single valwidth) (single (TxtVal valcoltxt)) ""
hdrstep cfg t ks@(key : keys) =
  if colStackAt cfg == Just key
  then hdrvalstep cfg t mempty ks  -- switch to column-stacking mode
  else
    let keyw = max (T.length key)
               $ maybe 0 (maxOf 0 . fmap T.length) (L.lookup key $ t ^. keyVals)
        mkhdr (hs, v) (HdrLine fmt hdrvals trailer) =
          ( HdrLine (fmtAddColLeft keyw fmt) (TxtVal v : hdrvals) trailer : hs , "")
    in reverse $ fst $ foldl mkhdr (mempty, key) $ hdrstep cfg t keys
         -- first line shows hdrval for non-colstack'd columns, others are blank

hdrvalstep :: PP.Pretty v => RenderConfig -> KVITable v -> KeySpec -> Keys -> [HeaderLine]
hdrvalstep cfg t steppath (key : []) =
  let titles = sortedKeyVals cfg t key
      cvalWidths kv = fmap (length . show . PP.pretty . snd) $
                      filter ((L.isSuffixOf (snoc steppath (key, kv))) . fst)
                      $ KVIT.toList t
      colWidth kv = let cvw = cvalWidths kv
                    in if hideBlankCols cfg && sum cvw == 0
                       then 0
                       else maxOf (T.length kv) cvw
      cwidths = fmap colWidth titles
      fmtcols = if equisizedCols cfg
                then (replicate (length cwidths) (maxOf 0 cwidths))
                else cwidths
  in single $ HdrLine (fmtLine $ fmtcols) (TxtVal <$> titles) key
hdrvalstep cfg t steppath (key : keys) =
  let vals = sortedKeyVals cfg t key
      subhdrsV v = hdrvalstep cfg t (snoc steppath (key,v)) keys
      subTtlHdrs = let subAtVal v = (T.length v, subhdrsV v)
                   in fmap subAtVal vals
      szexts = let subW (hl,sh) =
                     case sh of
                       [] -> (0, 0)  -- should never be the case
                       (sh0:_) ->
                         let sv = fmtWidth $ hdrFmt sh0
                         in if hideBlankCols cfg && (fmtEmptyCols $ hdrFmt sh0)
                            then (0, 0)
                            else (hl, sv)
               in fmap (uncurry max . subW) subTtlHdrs
      rsz_extsubhdrs = fmap hdrJoin $
                       L.transpose $
                       fmap (uncurry rsz_hdrstack) $
                       zip szhdrs $ fmap snd subTtlHdrs
      largest = maxOf 0 szexts
      szhdrs = if equisizedCols cfg && not (hideBlankCols cfg)
               then replicate (length vals) largest
               else szexts
      rsz_hdrstack s vhs = fmap (rsz_hdrs s) vhs
      rsz_hdrs hw (HdrLine (FmtLine c s j) v r) =
        let nzCols = L.filter (/= 0) c
            pcw = sum nzCols + ((perColOvhd + 1) * (length nzCols - 1))
            (ew,w0) = let l = length nzCols
                      in if l == 0 then (0,0)
                         else max 0 (hw - pcw) `divMod` length nzCols
            c' = fst $ foldl (\(c'',n) w -> (snoc c'' $ n+w, ew)) (mempty,ew+w0) c
        in HdrLine (FmtLine c' s j) v r
      hdrJoin hl = foldl hlJoin (HdrLine (fmtLine mempty) mempty "") hl
      hlJoin (HdrLine (FmtLine c s j) v _) (HdrLine (FmtLine c' _ _) v' r) =
        HdrLine (FmtLine (c<>c') s j) (v<>v') r
      tvals = CenterVal <$> vals
  in HdrLine (fmtLine szhdrs) tvals key : rsz_extsubhdrs
hdrvalstep _ _ _ [] = error "ASCII hdrvalstep with empty keys after matching colStackAt -- impossible"

renderSeq :: PP.Pretty v => RenderConfig -> FmtLine -> Keys -> KVITable v -> [Text]
renderSeq cfg fmt keys kvitbl = fmtRender fmt . snd <$> asciiRows keys mempty
  where
    filterBlank = if hideBlankRows cfg
                  then L.filter (not . all isNothing . snd)
                  else id
    asciiRows :: Keys -> KeySpec -> [ (Bool, [FmtVal]) ]
    asciiRows [] path =
      let v = KVIT.lookup' path kvitbl
          skip = case v of
                   Nothing -> hideBlankRows cfg
                   Just _  -> False
      in if skip then mempty
         else single $ (False, single $ maybe (TxtVal "") TxtVal (T.pack . show . PP.pretty <$> v) )
    asciiRows ks@(key : kseq) path
      | colStackAt cfg == Just key =
        let filterOrDefaultBlankRows = fmap (fmap defaultBlanks) . filterBlank
            defaultBlanks = fmap (\v -> maybe (TxtVal "") TxtVal v)
        in filterOrDefaultBlankRows $ single $ (False, multivalRows ks path)
      | otherwise =
        let subrows keyval = asciiRows kseq $ snoc path (key, keyval)
            grprow = \case
              subs@(sub0:_) | key `elem` rowGroup cfg ->
                  let subl = single (True, replicate (length $ snd sub0) Separator)
                  in if fst (last subs)
                     then init subs <> subl
                     else subs <> subl
              subs -> subs
            genSubRow keyval = grprow $ fst
                               $ foldl leftAdd (mempty, keyval) $ subrows keyval
            leftAdd (acc,kv) (b,subrow) = (snoc acc (b, TxtVal kv : subrow),
                                           if rowRepeat cfg then kv else "")
        in concat (genSubRow <$> (sortedKeyVals cfg kvitbl key))

    multivalRows :: Keys -> KeySpec -> [ Maybe Text ]
    multivalRows (key : []) path =
      let keyvals = sortedKeyVals cfg kvitbl key
          showEnt = T.pack . show . PP.pretty
      in (\v -> (showEnt <$> (KVIT.lookup' (snoc path (key,v)) kvitbl))) <$> keyvals
    multivalRows (key : kseq) path =
      let keyvals = sortedKeyVals cfg kvitbl key
      in concatMap (\v -> multivalRows kseq (snoc path (key,v))) keyvals
    multivalRows [] _ = error "multivalRows cannot be called with no keys!"
