{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides the 'KVITable' 'render' function for
-- rendering the table in a HTML table format.  The various HTML table
-- entries have class designators that allow the user to provide CSS
-- to adjust the appearance of the table.

module Data.KVITable.Render.HTML
  (
    render
    -- re-export Render definitions to save the caller an additional import
  , RenderConfig(..)
  , defaultRenderConfig
  )
where

import qualified Data.Foldable as F
import qualified Data.List as L
import           Data.List.NonEmpty ( NonEmpty( (:|) ) )
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe ( fromMaybe, isNothing )
import           Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Lens.Micro ( (^.) )
import           Lucid
import qualified Prettyprinter as PP

import           Data.KVITable
import           Data.KVITable.Render

import           Prelude hiding ( lookup )


-- | Renders the specified table in HTML format, using the specified
-- 'RenderConfig' controls.  The output is only the @<table>@
-- definition; it is intended to be embedded in a larger HTML
-- document.

render :: PP.Pretty v => RenderConfig -> KVITable v -> Text
render cfg t =
  let kseq = fst <$> t ^. keyVals
      (fmt, hdr) = renderHdrs cfg t kseq
      bdy = renderSeq cfg fmt kseq t
  in TL.toStrict $ renderText $
     table_ [ class_ "kvitable" ] $
     do maybe mempty (caption_ . toHtml) $ Data.KVITable.Render.caption cfg
        thead_ [ class_ "kvitable_head" ] hdr
        tbody_ [ class_ "kvitable_body" ] bdy

----------------------------------------------------------------------

data FmtLine = FmtLine [Int]  -- colspans, length is # columns

instance Semigroup FmtLine where
  (FmtLine c1) <> (FmtLine c2) = FmtLine $ c1 <> c2

instance Monoid FmtLine where
  mempty = FmtLine mempty

fmtAddColLeft :: Int -> FmtLine -> FmtLine
fmtAddColLeft lspan (FmtLine col) = FmtLine $ lspan : col

data FmtVal = Val Height LastInGroup Text
            | Hdr Height LastInGroup Text
            deriving Show
type Height = Int
type LastInGroup = Bool
type RightLabel = Text

fmtRender :: FmtLine -> [FmtVal] -> Maybe RightLabel -> Html ()
fmtRender (FmtLine cols) vals mbRLabel = do
  tr_ [ class_ "kvitable_tr" ] $
    let excessColCnt = length cols - length vals
        cell (w,Hdr h l v) =
          let a = [ [ class_ "kvitable_th" ]
                  , if h == 1 then []
                    else [ rowspan_ $ T.pack $ show h ]
                  , if w == 1 then []
                    else [ colspan_ $ T.pack $ show w
                         , class_ " multicol" ]
                  , if l then [ class_ " last_in_group" ] else []
                  ]
          in th_ (concat $ reverse a) (div_ $ span_ $ toHtml v)
        cell (w,Val h l v) =
          let a = [ [ class_ "kvitable_td" ]
                  , if h == 1 then []
                    else [ rowspan_ $ T.pack $ show h ]
                  , if w == 1 then []
                    else [ colspan_ $ T.pack $ show w
                         , class_ " multicol" ]
                  , if l then [ class_ " last_in_group" ] else []
                  ]
          in td_ (concat $ reverse a) (toHtml v)
        labelMark = toHtmlRaw ("&nbsp;&larr;" :: Text)
        labelHtml = th_ [ class_ "rightlabel kvitable_th" ] .
                    (labelMark <>) .
                    toHtml
    in do mapM_ cell $ L.filter ((/= 0) . fst) $
            zip (drop excessColCnt cols) vals
          maybe mempty labelHtml mbRLabel


----------------------------------------------------------------------

data HeaderLine = HdrLine FmtLine HdrVals Trailer
type HdrVals = [FmtVal]
type Trailer = Maybe Text

instance Semigroup HeaderLine where
  (HdrLine fmt1 hv1 t1) <> (HdrLine fmt2 hv2 _) =
    HdrLine (fmt1 <> fmt2) (hv1 <> hv2) t1

hdrFmt :: HeaderLine -> FmtLine
hdrFmt (HdrLine fmt _ _) = fmt

renderHdrs :: PP.Pretty v
           => RenderConfig -> KVITable v -> [Key]
           -> ( FmtLine, Html () )
renderHdrs cfg t keys = ( rowfmt, sequence_ hdrs )
  where
    hdrs = fmap renderHdr hrows
    (hrows, rowfmt) = hdrstep cfg t keys
    renderHdr (HdrLine fmt hdrvals trailer) = fmtRender fmt hdrvals trailer

hdrstep :: PP.Pretty v
        => RenderConfig -> KVITable v -> [Key]
        -> (NEL.NonEmpty HeaderLine, FmtLine)
hdrstep _cfg t [] =
  ( HdrLine (FmtLine [1]) [Hdr 1 False $ t ^. valueColName] Nothing :| []
  , FmtLine [1]
  )
hdrstep cfg t (key:keys) =
  if colStackAt cfg == Just key
  then hdrvalstep cfg t [] (key:keys) -- switch to column stacking mode
  else
    let (nexthdr0 :| nexthdrs, lowestfmt) = hdrstep cfg t keys
        (HdrLine fmt vals tr) = nexthdr0
        fmt' = fmtAddColLeft 1 fmt
        val = Hdr (length nexthdrs + 1) False key
    in ( (HdrLine fmt' (val : vals) tr) :| nexthdrs
       , fmtAddColLeft 1 lowestfmt
       )

hdrvalstep :: PP.Pretty v
           => RenderConfig -> KVITable v -> KeySpec -> [Key]
           -> (NEL.NonEmpty HeaderLine, FmtLine)
hdrvalstep _ _ _ [] = error "HTML hdrvalstep with empty keys after matching colStackAt -- impossible"
hdrvalstep cfg t steppath (key:[]) =
  let titles = ordering $ fromMaybe [] $ L.lookup key $ t ^. keyVals
      ordering = if sortKeyVals cfg then sortWithNums else id
      cvalWidths kv = fmap (length . show . PP.pretty . snd) $
                      L.filter ((L.isSuffixOf (steppath <> [(key, kv)])) . fst) $
                      toList t
      cwidth c = if and [ hideBlankCols cfg
                        , 0 == (sum $ cvalWidths c) ]
                 then 0
                 else 1
      fmt = FmtLine $ fmap cwidth titles
  in ( HdrLine fmt (Hdr 1 False <$> titles) (Just key) :| [], fmt)
hdrvalstep cfg t steppath (key:keys) =
  let ordering = if sortKeyVals cfg then sortWithNums else id
  in case ordering $ fromMaybe [] $ L.lookup key $ t ^. keyVals of
       [] -> error "cannot happen"
       (ttl:ttls) ->
         let
           titles = ttl :| ttls
           subhdrsV v = hdrvalstep cfg t (steppath <> [(key,v)]) keys
           subTtlHdrs :: NEL.NonEmpty (NEL.NonEmpty HeaderLine, FmtLine)
           subTtlHdrs = subhdrsV <$> titles
           subhdrs :: NEL.NonEmpty (NEL.NonEmpty HeaderLine, FmtLine)
           subhdrs = if hideBlankCols cfg
                     then subTtlHdrs
                     else
                       -- Want to repeat the first element of subTtlHdrs to get a
                       -- NonEmpty the same length as titles.  Both titles and
                       -- subTtlHdrs are NonEmpty, but NonEmpty has no replicate
                       -- function.
                       let n = length titles -- >= 1 because titles is NonEmpty
                           e = NEL.head subTtlHdrs
                           tail' = NEL.take (n-1) $ NEL.repeat e
                       in e :| tail'
           subhdr_rollup = joinHdrs <$> NEL.transpose (fst <$> subhdrs)
           joinHdrs :: NEL.NonEmpty HeaderLine -> HeaderLine
           joinHdrs (hl0 :| hls) = foldl (<>) hl0 hls
           superFmt :: (NEL.NonEmpty HeaderLine, FmtLine) -> Int
           superFmt sub = let FmtLine subcols = hdrFmt $ NEL.last $ fst sub
                          in if sum subcols == 0
                             then 0
                             else length $ L.filter (/= 0) subcols
           topfmt = FmtLine $ NEL.toList (superFmt <$> subhdrs)
           tophdr = HdrLine topfmt (NEL.toList (Hdr 1 False <$> titles)) $ Just key
         in ( NEL.cons tophdr subhdr_rollup, F.fold (snd <$> subTtlHdrs))

----------------------------------------------------------------------

renderSeq :: PP.Pretty v
          => RenderConfig -> FmtLine -> [Key] -> KVITable v -> Html ()
renderSeq cfg fmt keys t =
  mapM_ (flip (fmtRender fmt) Nothing) $ htmlRows keys []
  where
    mkVal = Val 1 False . T.pack . show . PP.pretty
    htmlRows :: [Key] -> KeySpec -> [ [FmtVal] ]
    htmlRows [] path =
      let v = lookup path t
          skip = case v of
            Nothing -> hideBlankRows cfg
            Just _ -> False
          row = maybe (Val 1 False "") mkVal v
      in if skip then [] else [ [row] ]
    htmlRows (key:kseq) path
      | colStackAt cfg == Just key =
          let filterOrDefaultBlankRows =
                fmap (fmap (maybe (Val 1 False "") id)) .
                if hideBlankRows cfg
                then L.filter (not . all isNothing)
                else id
          in filterOrDefaultBlankRows $
             [ multivalRows (key:kseq) path ]
      | otherwise =
          let keyvals = ordering $ fromMaybe [] $ L.lookup key $ t ^. keyVals
              ordering = if sortKeyVals cfg then sortWithNums else id
              subrows keyval = htmlRows kseq $ path <> [(key,keyval)]
              endOfGroup = key `elem` rowGroup cfg
              addSubrows ret keyval =
                let sr = subrows keyval
                in ret <> (fst $
                           foldl (leftAdd (length sr)) ([],Just keyval) $
                           reverse $ zip (endOfGroup: L.repeat False) $ reverse sr)
              leftAdd nrows (acc,mb'kv) (endGrp, subrow) =
                let sr = setValGrouping endGrp <$> subrow
                    setValGrouping g (Val h g' v) = Val h (g || g') v
                    setValGrouping g (Hdr h g' v) = Hdr h (g || g') v
                in ( acc <> [ (case mb'kv of
                                    Nothing -> sr
                                    Just kv -> let w = if rowRepeat cfg
                                                       then 1
                                                       else nrows
                                               in Hdr w endOfGroup kv : sr
                              ) ]
                   , if rowRepeat cfg then mb'kv else Nothing)
          in foldl addSubrows [] keyvals


    multivalRows [] _ = error "HTML multivalRows cannot be called with no keys!"
    multivalRows (key:[]) path =
      let keyvals = ordering $ fromMaybe [] $ L.lookup key $ t ^. keyVals
          ordering = if sortKeyVals cfg then sortWithNums else id
      in (\v -> mkVal <$> lookup (path <> [(key,v)]) t) <$> keyvals
    multivalRows (key:kseq) path =
      let keyvals = ordering $ fromMaybe [] $ L.lookup key $ t ^. keyVals
          ordering = if sortKeyVals cfg then sortWithNums else id
      in concatMap (\v -> multivalRows kseq (path <> [(key,v)])) keyvals
