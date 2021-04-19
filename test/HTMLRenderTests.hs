{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module HTMLRenderTests where

import           Control.Monad ( unless )
import           Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Lens.Micro ( (^.), (.~), (%~), (&) )
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.HTML.Parser ( parseTokens, renderToken, canonicalizeTokens )

import           SampleTables
import           TestQQDefs

import qualified Data.KVITable as KVI
import qualified Data.KVITable.Render as KTR
import qualified Data.KVITable.Render.HTML as KTRH


cmpTables :: Text -> Text -> Text -> IO ()
cmpTables nm actual expected = do
  let expH = parseTokens $ T.concat $ fmap T.strip $ T.lines expected
      actH = parseTokens actual
  unless (expH == actH) $ do
    let dl (e,a) = if e == a then db e else de " ↱" e <> "\n    " <> da " ↳" a
        db b = "|        > " <> b
        de m e = "|" <> m <> "expect> " <> e
        da m a = "|" <> m <> "actual> " <> a
        el = fmap (TL.toStrict . renderToken) $ canonicalizeTokens expH
        al = fmap (TL.toStrict . renderToken) $ canonicalizeTokens actH
        addnum n l = let nt = T.pack (show n)
                         nl = T.length nt
                     in T.take (4 - nl) "    " <> nt <> l
    let details = ("MISMATCH between " <>
                   T.pack (show $ length el) <> " expected and " <>
                   T.pack (show $ length al) <> " actual for " <> nm) :
                  (fmap (uncurry addnum) $
                   zip [1..] $ concat $
                   -- Highly simplistic "diff" output assumes
                   -- correlated lines: added or removed lines just
                   -- cause everything to shown as different from that
                   -- point forward.
                   [ fmap dl $ zip el al
                   , fmap (de "∌ ") $ drop (length al) el
                   , fmap (da "∹ ") $ drop (length el) al
                   ])
    assertFailure $ T.unpack $ T.unlines details


testHTMLRendering =
  testGroup "HTML rendering" $
  let kvi0 = mempty :: KVI.KVITable Text
      cfg0 = KTR.defaultRenderConfig
      cfgWBlankRows = cfg0 { KTR.hideBlankRows = False }
  in
    [
      testCase "empty table, hide blank" $
      cmpTables "empty table, hide blank"
      (KTRH.render cfg0 kvi0) [sq|
@@@@
<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr"><th class="kvitable_th">Value</th></tr>
  </thead>
  <tbody class="kvitable_body">
  </tbody>
</table>
@@@@
|]

    , testCase "empty table, show blank" $
      cmpTables "empty table, show blank"
      (KTRH.render cfgWBlankRows kvi0) [sq|
@@@@
<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr"><th class="kvitable_th">Value</th></tr>
  </thead>
  <tbody class="kvitable_body">
    <tr class="kvitable_tr">
      <td class="kvitable_td"></td>
    </tr>
  </tbody>
</table>
@@@@
|]

    , testCase "empty table with labels" $
      let kvi = mempty & KVI.keyVals @Float .~ [ ("foo", []), ("dog", []) ]
      in cmpTables "empty table with labels" (KTRH.render cfg0 kvi) [sq|
@@@@
<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr">
      <th class="kvitable_th">foo</th>
      <th class="kvitable_th">dog</th>
      <th class="kvitable_th">Value</th>
    </tr>
  </thead>
  <tbody class="kvitable_body">
  </tbody>
</table>
@@@@
|]

    , testCase "nested table hideBlank=rows,cols, fitted, colstack=hundreds" $
      cmpTables "nested table hideBlank=rows,cols, fitted, colstack=hundreds"
      (KTRH.render (cfg0 { KTR.sortKeyVals     = True
                         , KTR.rowRepeat     = False
                         , KTR.hideBlankCols = True
                         , KTR.hideBlankRows = True
                         , KTR.equisizedCols = False
                         , KTR.colStackAt    = Just "hundreds"
                         }) nestedTable)
        [sq|
ASCII form:
| millions | thousands | ___ 1 ____ | ___ 2 ____ | <- hundreds
|          |           | ___ 2 ____ | ___ 2 ____ | <- tens
|          |           |    0 |   1 |    0 |   1 | <- ones
+----------+-----------+------+-----+------+-----+
|        0 |         0 | even | odd | even | odd |
|          |         1 | even | odd | even | odd |
|          |         2 | even | odd | even | odd |
|        1 |         0 | even | odd | even | odd |
|          |         1 | even | odd | even | odd |
|          |         2 | even | odd | even | odd |
|        2 |         0 | even | odd | even | odd |
|          |         1 | even | odd | even | odd |
|          |         2 | even | odd | even | odd |
@@@@
<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr">
      <th rowspan="3" class="kvitable_th">millions</th>
      <th rowspan="3" class="kvitable_th">thousands</th>
      <th colspan="2" class="kvitable_th multicol">1</th>
      <th colspan="2" class="kvitable_th multicol">2</th>
    </tr><span>&nbsp;&larr;hundreds</span>
    <tr class="kvitable_tr">
      <th colspan="2" class="kvitable_th multicol">2</th>
      <th colspan="2" class="kvitable_th multicol">2</th>
    </tr><span>&nbsp;&larr;tens</span>
    <tr class="kvitable_tr">
      <th class="kvitable_th">0</th>
      <th class="kvitable_th">1</th>
      <th class="kvitable_th">0</th>
      <th class="kvitable_th">1</th>
    </tr><span>&nbsp;&larr;ones</span>
  </thead>
  <tbody class="kvitable_body">
    <tr class="kvitable_tr">
      <th rowspan="3" class="kvitable_th">0</th>
      <th class="kvitable_th">0</th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th">1</th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th">2</th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="3" class="kvitable_th">1</th>
      <th class="kvitable_th">0</th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th">1</th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th">2</th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="3" class="kvitable_th">2</th>
      <th class="kvitable_th">0</th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th">1</th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th">2</th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
  </tbody>
</table>
@@@@
|]

    , testCase "nested table hide=none, fitted, colstack=hundreds" $
      cmpTables "nested table hide=none, fitted, colstack=hundreds"
      (KTRH.render (cfg0 { KTR.sortKeyVals   = True
                         , KTR.rowRepeat     = False
                         , KTR.hideBlankCols = False
                         , KTR.hideBlankRows = False
                         , KTR.equisizedCols = False
                         , KTR.colStackAt    = Just "hundreds"
                         }) nestedTable)
        [sq|
ASCII form:
| millions | thousands | _____ 0 _____ | _______ 1 ________ | _______ 2 ________ | <- hundreds
|          |           | _ 0 _ | _ 2 _ | _ 0 _ | ___ 2 ____ | _ 0 _ | ___ 2 ____ | <- tens
|          |           | 0 | 1 | 0 | 1 | 0 | 1 |    0 |   1 | 0 | 1 |    0 |   1 | <- ones
+----------+-----------+---+---+---+---+---+---+------+-----+---+---+------+-----+
|        0 |         0 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         1 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         2 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|        1 |         0 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         1 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         2 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|        2 |         0 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         1 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         2 |   |   |   |   |   |   | even | odd |   |   | even | odd |
@@@@
<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr">
      <th rowspan="3" class="kvitable_th">millions</th>
      <th rowspan="3" class="kvitable_th">thousands</th>
      <th colspan="4" class="kvitable_th multicol">0</th>
      <th colspan="4" class="kvitable_th multicol">1</th>
      <th colspan="4" class="kvitable_th multicol">2</th>
      <th class="rightlabel kvitable_th">&nbsp;&larr;hundreds</th>
    </tr>
    <tr class="kvitable_tr">
      <th colspan="2" class="kvitable_th multicol">0</th>
      <th colspan="2" class="kvitable_th multicol">2</th>
      <th colspan="2" class="kvitable_th multicol">0</th>
      <th colspan="2" class="kvitable_th multicol">2</th>
      <th colspan="2" class="kvitable_th multicol">0</th>
      <th colspan="2" class="kvitable_th multicol">2</th>
      <th class="rightlabel kvitable_th">&nbsp;&larr;tens</th>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th">0</th>
      <th class="kvitable_th">1</th>
      <th class="kvitable_th">0</th>
      <th class="kvitable_th">1</th>
      <th class="kvitable_th">0</th>
      <th class="kvitable_th">1</th>
      <th class="kvitable_th">0</th>
      <th class="kvitable_th">1</th>
      <th class="kvitable_th">0</th>
      <th class="kvitable_th">1</th>
      <th class="kvitable_th">0</th>
      <th class="kvitable_th">1</th>
      <th class="rightlabel kvitable_th">&nbsp;&larr;ones</th>
    </tr>
  </thead>
  <tbody class="kvitable_body">
    <tr class="kvitable_tr">
      <th rowspan="3" class="kvitable_th">0</th>
      <th class="kvitable_th">0</th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th">1</th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th">2</th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="3" class="kvitable_th">1</th>
      <th class="kvitable_th">0</th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th">1</th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th">2</th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="3" class="kvitable_th">2</th>
      <th class="kvitable_th">0</th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th">1</th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th">2</th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
  </tbody>
</table>
@@@@|]

    ]
