{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.KVITable.Render.HTML
  (
    render
  )
where

import qualified Data.Foldable as F
import qualified Data.List as L
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

data FmtVal = Val Height Text | Hdr Height Text
type Height = Int
type RightLabel = Text

fmtRender :: FmtLine -> [FmtVal] -> Maybe RightLabel -> Html ()
fmtRender (FmtLine cols) vals mbRLabel = do
  tr_ [ class_ "kvitable_tr" ] $
    let excessColCnt = length cols - length vals
        cell (w,Hdr h v) = let a = [ [ class_ "kvitable_th" ]
                                   , if h == 1 then []
                                     else [ rowspan_ $ T.pack $ show h ]
                                   , if w == 1 then []
                                     else [ colspan_ $ T.pack $ show w
                                          , class_ " multicol" ]
                                   ]
                           in th_ (concat $ reverse a) (toHtml v)
        cell (w,Val h v) = let a = [ [ class_ "kvitable_td" ]
                                   , if h == 1 then []
                                     else [ rowspan_ $ T.pack $ show h ]
                                   , if w == 1 then []
                                     else [ colspan_ $ T.pack $ show w
                                          , class_ " multicol" ]
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
renderHdrs cfg t keys =
  ( rowfmt, sequence_ [ fmtRender fmt hdrvals trailer
                      | (HdrLine fmt hdrvals trailer) <- hrows
                      ])
  where
    (hrows, rowfmt) = hdrstep cfg t keys

hdrstep :: PP.Pretty v
        => RenderConfig -> KVITable v -> [Key] -> ([HeaderLine], FmtLine)
hdrstep _cfg t [] =
  ( [ HdrLine (FmtLine [1]) [Hdr 1 $ t ^. valueColName] Nothing ]
  , FmtLine [1]
  )
hdrstep cfg t (key:keys) =
  if colStackAt cfg == Just key
  then hdrvalstep cfg t [] (key:keys) -- switch to column stacking mode
  else
    let (nexthdrs, lowestfmt) = hdrstep cfg t keys
        (HdrLine fmt vals tr) = head nexthdrs -- safe: there were keys
        fmt' = fmtAddColLeft 1 fmt
        val = Hdr (length nexthdrs) key
    in ( (HdrLine fmt' (val : vals) tr) : tail nexthdrs
       , fmtAddColLeft 1 lowestfmt
       )

hdrvalstep :: PP.Pretty v
           => RenderConfig -> KVITable v -> KeySpec -> [Key]
           -> ([HeaderLine], FmtLine)
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
  in ( [ HdrLine fmt (Hdr 1 <$> titles) (Just key) ], fmt)
hdrvalstep cfg t steppath (key:keys) =
  let titles = ordering $ fromMaybe [] $ L.lookup key $ t ^. keyVals
      ordering = if sortKeyVals cfg then sortWithNums else id
      subhdrsV v = hdrvalstep cfg t (steppath <> [(key,v)]) keys
      subTtlHdrs :: [ ([HeaderLine], FmtLine) ]
      subTtlHdrs = subhdrsV <$> titles
      subhdrs = if hideBlankCols cfg
                then subTtlHdrs
                else L.replicate (length titles) $ head subTtlHdrs
      subhdr_rollup = joinHdrs <$> L.transpose (fst <$> subhdrs)
      joinHdrs hl = foldl (<>) (head hl) (tail hl)
      superFmt sub = let FmtLine subcols = hdrFmt $ last $ fst sub
                     in if sum subcols == 0
                        then 0
                        else length $ L.filter (/= 0) subcols
      topfmt = FmtLine (superFmt <$> subhdrs)
      tophdr = HdrLine topfmt (Hdr 1 <$> titles) $ Just key
  in ( tophdr : subhdr_rollup, F.fold (snd <$> subTtlHdrs))

----------------------------------------------------------------------

renderSeq :: PP.Pretty v
          => RenderConfig -> FmtLine -> [Key] -> KVITable v -> Html ()
renderSeq cfg fmt keys t =
  mapM_ (flip (fmtRender fmt) Nothing . snd) $ htmlRows keys []
  where
    mkVal ty = ty 1 . T.pack . show . PP.pretty
    htmlRows :: [Key] -> KeySpec -> [ (Bool, [FmtVal]) ]
    htmlRows [] path =
      let v = lookup path t
          skip = case v of
            Nothing -> hideBlankRows cfg
            Just _ -> False
          row = maybe (Val 1 "") (mkVal Val) v
      in if skip then [] else [ (False, [row]) ]
    htmlRows (key:kseq) path
      | colStackAt cfg == Just key =
          let filterOrDefaultBlankRows =
                fmap (fmap (fmap (maybe (Val 1 "") id))) .
                if hideBlankRows cfg
                then L.filter (not . all isNothing . snd)
                else id
          in filterOrDefaultBlankRows $
             [ (False, multivalRows (key:kseq) path) ]
      | otherwise =
          let keyvals = ordering $ fromMaybe [] $ L.lookup key $ t ^. keyVals
              ordering = if sortKeyVals cfg then sortWithNums else id
              subrows keyval = htmlRows kseq $ path <> [(key,keyval)]
              -- grprow subs = if key `elem` rowGroup cfg && not (null subs)
              --               then undefined
              --               else subs
              addSubrows ret keyval =
                let sr = subrows keyval
                in ret <> (fst $ foldl (leftAdd (length sr)) ([],Just keyval) $ sr)
              leftAdd nrows (acc,mb'kv) (b,subrow) =
                ( acc <> [ (b, case mb'kv of
                                 Nothing -> subrow
                                 Just kv -> Hdr (if rowRepeat cfg then 1 else nrows) kv : subrow) ]
                , if rowRepeat cfg then mb'kv else Nothing)
          in foldl addSubrows [] keyvals


    -- multivalRows :: [Key] -> KeySpec -> [ Maybe v ]
    multivalRows [] _ = error "HTML multivalRows cannot be called with no keys!"
    multivalRows (key:[]) path =
      let keyvals = ordering $ fromMaybe [] $ L.lookup key $ t ^. keyVals
          ordering = if sortKeyVals cfg then sortWithNums else id
      in (\v -> mkVal Val <$> lookup (path <> [(key,v)]) t) <$> keyvals
    multivalRows (key:kseq) path =
      let keyvals = ordering $ fromMaybe [] $ L.lookup key $ t ^. keyVals
          ordering = if sortKeyVals cfg then sortWithNums else id
      in concatMap (\v -> multivalRows kseq (path <> [(key,v)])) keyvals
