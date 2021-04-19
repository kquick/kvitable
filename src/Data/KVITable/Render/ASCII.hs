{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.KVITable.Render.ASCII
  (
    render
  )
where

import           Control.Applicative ( (<|>) )
import qualified Data.List as L
import           Data.Maybe ( fromMaybe )
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

fmtAddColLeft :: Int -> FmtLine -> FmtLine
fmtAddColLeft leftCol (FmtLine cols s s') = FmtLine (leftCol : cols) s s'

fmtRender :: FmtLine -> [Maybe Text] -> Text
fmtRender (FmtLine [] _sigils _sepsigils) [] = ""
fmtRender (FmtLine cols sigils sepsigils) vals =
  if length cols == length vals
  then let sig f o = case o of
                       Nothing -> f sepsigils
                       Just  _ -> f sigils
           l = sig sep $ head vals
       in l <> T.concat [ sig pad fld <>
                          (case fld of
                             Nothing -> T.pack (replicate sz '-')
                             Just  v -> T.pack (replicate (sz - T.length v) ' ') <> v  -- KWQ: CenterCap, CenterCap.render(sz, cap sigils)
                          ) <>
                          sig pad fld <>
                          sig sep fld  -- KWQ or if next fld is Nothing
                        | (sz,fld) <- zip cols vals
                        ]
  else error ("Insufficient arguments (" <>
              show (length vals) <> ")" <>
              " for FmtLine " <> show (length cols) <>
              " on " <> show vals)

----------------------------------------------------------------------

data HeaderLine = HdrLine FmtLine HdrVals Trailer
type HdrVals = [Text]
type Trailer = Text

hdrFmt :: HeaderLine -> FmtLine
hdrFmt (HdrLine fmt _ _) = fmt

renderHdrs :: PP.Pretty v => RenderConfig -> KVITable v -> [Key] -> (FmtLine, [Text])
renderHdrs cfg t keys =
  ( lastFmt
  , [ fmtRender fmt (Just . T.pack . show . PP.pretty <$> hdrvals)
      <> (if T.null trailer then "" else (" <- " <> trailer))
    | (HdrLine fmt hdrvals trailer) <- hrows
    ] <>
    [ fmtRender lastFmt (replicate (fmtColCnt lastFmt) Nothing) ])
  where
    hrows = hdrstep cfg t keys
    lastFmt = if null hrows then fmtLine [] else hdrFmt $ head $ reverse hrows
-- renderHdrs t keys =
--   let hrows = hdrstep t keys
--       lastFmt = if null hrows then fmtLine [] else hdrFmt $ head $ reverse hrows
--   in ( lastFmt
--      , [ fmtRender fmt (Just <$> hdrvals)
--          <> (if T.null trailer then "" else (" <- " <> trailer))
--        | (HdrLine fmt hdrvals trailer) <- hrows
--        ] <>
--        [ fmtRender lastFmt (replicate (fmtColCnt lastFmt) (Nothing @v)) ])

hdrstep :: PP.Pretty v => RenderConfig -> KVITable v -> [Key] -> [HeaderLine]
hdrstep _cfg t [] =
  -- colStackAt wasn't recognized, so devolve into a non-colstack table
  let valcoltxt = t ^. valueColName
      valcoltsz = T.length valcoltxt
      valsizes  = length . show . PP.pretty . snd <$> toList t
      valwidth  = maximum $ valcoltsz : valsizes
  in [ HdrLine (fmtLine [valwidth]) [valcoltxt] "" ]
hdrstep cfg t (key:keys) =
  if colStackAt cfg == Just key
  then hdrvalstep (key:keys)  -- switch to column-stacking mode
  else
    let keyw = maximum $ (T.length key) :
               fmap T.length (fromMaybe [] $ L.lookup key $ t ^. keyVals)
        mkhdr (hs, v) (HdrLine fmt hdrvals trailer) =
          ( HdrLine (fmtAddColLeft keyw fmt) (v : hdrvals) trailer : hs , "")
    in reverse $ fst $ foldl mkhdr ([], key) $ hdrstep cfg t keys
         -- first line shows hdrval for non-colstack'd columns, others are blank

hdrvalstep :: [Key] -> [HeaderLine]
hdrvalstep _keys = undefined

renderSeq :: PP.Pretty v => RenderConfig -> FmtLine -> [Key] -> KVITable v -> [Text]
renderSeq cfg fmt keys kvitbl = fmtRender fmt . snd <$> asciiRows keys []
  where
    asciiRows :: [Key] -> KeySpec -> [ (Bool, [Maybe Text]) ]
    asciiRows [] path =
      let v = lookup path kvitbl
          skip = case v of
                   Nothing -> hideBlankRows cfg
                   Just _ -> False
      in if skip then []
         else [ (False, [ T.pack . show . PP.pretty <$> v <|> Just "" ]) ]
    asciiRows (key:kseq) path
      | colStackAt cfg == Just key = [ (False, multivalRows (key:kseq) path) ]
      | otherwise =
        let subrows keyval = asciiRows kseq $ path <> [ (key, keyval) ]
            grprow keyval subs = if key `elem` rowGroup cfg && not (null subs)
                                 then let subl = [ (True, replicate (length $ snd $ head subs) Nothing) ]
                                      in if fst (last subs)
                                         then init subs <> subl
                                         else subs <> subl
                                 else subs
            addSubrows ret keyval = ret <> (grprow keyval $ fst $
                                            foldl leftAdd ([],keyval) $ subrows keyval)
            leftAdd (acc,kv) (b,subrow) = (acc <> [ (b, Just kv : subrow) ],
                                           if rowRepeat cfg then kv else "")
            ordering = if sortKeyVals cfg then sortWithNums else id
        in foldl addSubrows [] $ ordering $ fromMaybe [] $ L.lookup key $ kvitbl ^. keyVals
    multivalRows kseq path = undefined -- [ Just "Hi" ]


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
