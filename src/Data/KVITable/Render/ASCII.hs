{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.KVITable.Render.ASCII
  (
    render
  )
where

import qualified Data.List as L
import           Data.Maybe ( fromMaybe, isNothing )
import           Data.Text ( Text )
import qualified Data.Text as T
import           Lens.Micro ( (^.) )
import qualified Prettyprinter as PP

import           Data.KVITable
import           Data.KVITable.Render

import           Prelude hiding ( lookup )


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
fmtWidth :: FmtLine -> Int
fmtWidth (FmtLine cols _ _) = sum cols + ((perColOvhd + 1) * (length cols - 1))

fmtAddColLeft :: Int -> FmtLine -> FmtLine
fmtAddColLeft leftCol (FmtLine cols s s') = FmtLine (leftCol : cols) s s'

data FmtVal = Separator | TxtVal Text | CenterVal Text

fmtRender :: FmtLine -> [FmtVal] -> Text
fmtRender (FmtLine [] _sigils _sepsigils) [] = ""
fmtRender (FmtLine cols sigils sepsigils) vals =
  if length cols == length vals
  then let sig f o = case o of
                       Separator   -> f sepsigils
                       TxtVal _    -> f sigils
                       CenterVal _ -> f sigils
           l = sig sep $ head vals
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
          | (sz,fld) <- zip cols vals
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

renderHdrs :: PP.Pretty v => RenderConfig -> KVITable v -> [Key] -> (FmtLine, [Text])
renderHdrs cfg t keys =
  ( lastFmt
  , [ fmtRender fmt hdrvals
      <> (if T.null trailer then "" else (" <- " <> trailer))
    | (HdrLine fmt hdrvals trailer) <- hrows
    ] <>
    [ fmtRender lastFmt (replicate (fmtColCnt lastFmt) Separator) ])
  where
    hrows = hdrstep cfg t keys
    lastFmt = if null hrows then fmtLine [] else hdrFmt $ head $ reverse hrows

hdrstep :: PP.Pretty v => RenderConfig -> KVITable v -> [Key] -> [HeaderLine]
hdrstep _cfg t [] =
  -- colStackAt wasn't recognized, so devolve into a non-colstack table
  let valcoltxt = t ^. valueColName
      valcoltsz = T.length valcoltxt
      valsizes  = length . show . PP.pretty . snd <$> toList t
      valwidth  = maximum $ valcoltsz : valsizes
  in [ HdrLine (fmtLine [valwidth]) [TxtVal valcoltxt] "" ]
hdrstep cfg t (key:keys) =
  if colStackAt cfg == Just key
  then hdrvalstep cfg t [] (key:keys)  -- switch to column-stacking mode
  else
    let keyw = maximum ( T.length key :
                         fmap T.length (fromMaybe [] $ L.lookup key $ t ^. keyVals) )
        mkhdr (hs, v) (HdrLine fmt hdrvals trailer) =
          ( HdrLine (fmtAddColLeft keyw fmt) (TxtVal v : hdrvals) trailer : hs , "")
    in reverse $ fst $ foldl mkhdr ([], key) $ hdrstep cfg t keys
         -- first line shows hdrval for non-colstack'd columns, others are blank

hdrvalstep :: PP.Pretty v => RenderConfig -> KVITable v -> KeySpec -> [Key] -> [HeaderLine]
hdrvalstep _ _ _ [] = error "ASCII hdrvalstep with empty keys after matching colstackAt -- impossible"
hdrvalstep cfg t steppath (key:[]) =
  let titles = ordering $ fromMaybe [] $ L.lookup key $ t ^. keyVals
      ordering = if sortKeyVals cfg then sortWithNums else id
      colWidth kv = maximum ( T.length kv :
                              ( fmap (length . show . PP.pretty . snd) $
                                L.filter ((L.isSuffixOf (steppath <> [(key, kv)])) . fst) $
                                toList t))
      cwidths = fmap colWidth titles
      fmtcols = if equisizedCols cfg
                then (replicate (length cwidths) (maximum cwidths))
                else cwidths
  in [ HdrLine (fmtLine fmtcols) (TxtVal <$> titles) key ]
hdrvalstep cfg t steppath (key:keys) =
  let vals = ordering $ fromMaybe [] $ L.lookup key $ t ^. keyVals
      ordering = if sortKeyVals cfg then sortWithNums else id
      subhdrsV v = hdrvalstep cfg t (steppath <> [(key,v)]) keys
      subTtlHdrs = fmap (\v -> (T.length v, subhdrsV v)) vals
      szexts = fmap (uncurry max . fmap (fmtWidth . hdrFmt . head)) subTtlHdrs
      rsz_extsubhdrs = fmap hdrJoin $
                       L.transpose $
                       fmap (uncurry rsz_hdrstack) $
                       zip szhdrs $ fmap snd subTtlHdrs
      largest = maximum szexts
      szhdrs = if equisizedCols cfg
               then replicate (length vals) largest
               else szexts
      rsz_hdrstack s vhs = fmap (rsz_hdrs s) vhs
      rsz_hdrs hw (HdrLine (FmtLine c s j) v r) =
        let pcw = sum c + ((perColOvhd + 1) * (length c - 1))
            (ew,w0) = (hw - pcw) `divMod` length c
            c' = fst $ foldl (\(c'',n) w -> (c''<>[n+w],ew)) ([],ew+w0) c
        in HdrLine (FmtLine c' s j) v r
      hdrJoin hl = foldl hlJoin (HdrLine (fmtLine []) [] "") hl
      hlJoin (HdrLine (FmtLine c s j) v _) (HdrLine (FmtLine c' _ _) v' r) =
        HdrLine (FmtLine (c<>c') s j) (v<>v') r
      tvals = fmap CenterVal vals
  in HdrLine (fmtLine szhdrs) tvals key : rsz_extsubhdrs

renderSeq :: PP.Pretty v => RenderConfig -> FmtLine -> [Key] -> KVITable v -> [Text]
renderSeq cfg fmt keys kvitbl = fmtRender fmt . snd <$> asciiRows keys []
  where
    asciiRows :: [Key] -> KeySpec -> [ (Bool, [FmtVal]) ]
    asciiRows [] path =
      let v = lookup path kvitbl
          skip = case v of
                   Nothing -> hideBlankRows cfg
                   Just _  -> False
      in if skip then []
         else [ (False, [ maybe (TxtVal "") TxtVal (T.pack . show . PP.pretty <$> v) ]) ]
    asciiRows (key:kseq) path
      | colStackAt cfg == Just key =
        let filterOrDefaultBlankRows =
              fmap (fmap defaultBlanks) .
              if hideBlankRows cfg
              then L.filter (not . all isNothing . snd)
              else id
            defaultBlanks = fmap (\v -> maybe (TxtVal "") TxtVal v)
        in filterOrDefaultBlankRows $ [ (False, multivalRows (key:kseq) path) ]
      | otherwise =
        let subrows keyval = asciiRows kseq $ path <> [ (key, keyval) ]
            grprow subs = if key `elem` rowGroup cfg && not (null subs)
                          then let subl = [ (True, replicate (length $ snd $ head subs) Separator) ]
                               in if fst (last subs)
                                  then init subs <> subl
                                  else subs <> subl
                          else subs
            addSubrows ret keyval = ret <> (grprow $ fst $
                                            foldl leftAdd ([],keyval) $ subrows keyval)
            leftAdd (acc,kv) (b,subrow) = (acc <> [ (b, TxtVal kv : subrow) ],
                                           if rowRepeat cfg then kv else "")
            ordering = if sortKeyVals cfg then sortWithNums else id
        in foldl addSubrows [] $ ordering $ fromMaybe [] $ L.lookup key $ kvitbl ^. keyVals
    multivalRows :: [Key] -> KeySpec -> [ Maybe Text ]
    multivalRows (key:[]) path =
      let ordering = if sortKeyVals cfg then sortWithNums else id
          keyvals = ordering $ fromMaybe [] $ L.lookup key $ kvitbl ^. keyVals
          showEnt = T.pack . show . PP.pretty
      in (\v -> (showEnt <$> (lookup (path <> [(key,v)]) kvitbl))) <$> keyvals
    multivalRows (key:kseq) path =
      let ordering = if sortKeyVals cfg then sortWithNums else id
          keyvals = ordering $ fromMaybe [] $ L.lookup key  $ kvitbl ^. keyVals
      in concatMap (\v -> multivalRows kseq (path <> [(key,v)])) keyvals
    multivalRows [] _ = error "multivalRows cannot be called with no keys!"


-- Sorting for KeyVals.  If the value starts or ends with a digit,
-- then this should do a rough numeric sort on the expectation that
-- the digits represent a version or some other numeric value.  As an
-- approximation of a numeric sort, sort by word size and then string
-- value.  This will result in [ "1", "2", "10", "50", "400" ], but
-- would fail with [ "v1.0", "v2.0", "v3.0", "v2.0.5", "v1.0.0.3" ],
-- but it's a reasonably fast heuristic and probably better than a
-- straight ascii sort.

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
