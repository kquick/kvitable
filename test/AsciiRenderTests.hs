{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module AsciiRenderTests where

import           Control.Monad ( unless )
import           Data.Text ( Text )
import qualified Data.Text as T
import           Lens.Micro ( (^.), (.~), (%~), (&) )
import           Test.Tasty
import           Test.Tasty.HUnit

import           SampleTables
import           TestQQDefs

import qualified Data.KVITable as KVI
import qualified Data.KVITable.Render as KTR
import qualified Data.KVITable.Render.ASCII as KTRA


cmpTables :: Text -> Text -> Text -> IO ()
cmpTables nm actual expected =
  unless (expected == actual) $ do
    let dl (e,a) = if e == a then db e else de " ↱" e <> "\n    " <> da " ↳" a
        db b = "|        > " <> b
        de m e = "|" <> m <> "expect> " <> e
        da m a = "|" <> m <> "actual> " <> a
        el = T.lines expected
        al = T.lines actual
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



testAsciiRendering =
  testGroup "ASCII rendering" $
  let kvi0 = mempty :: KVI.KVITable Text
      cfg0 = KTR.defaultRenderConfig
      cfgWBlankRows = cfg0 { KTR.hideBlankRows = False }
  in
    [
      testCase "empty table" $
      cmpTables "empty table" (KTRA.render cfg0 kvi0) [sq|
@@@@
| Value |
+-------+
@@@@
|]

    , testCase "empty table with blanks" $
      cmpTables "empty table" (KTRA.render cfgWBlankRows kvi0) [sq|
@@@@
| Value |
+-------+
|       |
@@@@
|]

    , testCase "empty table with labels" $
      let kvi = mempty & KVI.keyVals @Float .~ [ ("foo", []), ("dog", []) ]
      in cmpTables "empty table with labels" (KTRA.render cfg0 kvi) [sq|
@@@@
| foo | dog | Value |
+-----+-----+-------+
@@@@|]

    , testCase "add key" $
      let t0 = mempty & KVI.keyVals .~ [ ("foo", ["bar", "baz"]) ] :: KVI.KVITable Text
          t1 = KVI.insert [ ("foo", "baz"), ("dog", "woof") ] "yo" $
               KVI.insert [ ("foo", "bar") ] "hi"
               t0
      in do KVI.rows t1 @?= [ ([ "bar", "" ], "hi")
                        , ([ "baz", "woof"], "yo")
                        ]
            cmpTables "add key" (KTRA.render cfg0 t1) [sq|
@@@@
| foo |  dog | Value |
+-----+------+-------+
| bar |      |    hi |
| baz | woof |    yo |
@@@@|]


    , testCase "deep add" $
      let t0 = mempty
               & KVI.keyVals .~ [ ("foo", ["bar", "baz"])
                                , ("moon", ["beam", "pie"])
                                ]
               & KVI.valueColName .~ "says"
               & KVI.keyValGen .~ const "?"
               :: KVI.KVITable Text
          t1 = KVI.insert [ ("foo", "Bill"), ("moon", "Ted"), ("dog", "arf arf") ] "Excellent!" $
               KVI.insert [ ("foo", "baz"), ("moon", "beam"), ("dog", "woof") ] "yo" $
               KVI.insert [ ("foo", "bar"), ("moon", "pie") ] "hi"
               t0
      in cmpTables "add key" (KTRA.render cfg0 t1) [sq|
@@@@
|  foo | moon |     dog |       says |
+------+------+---------+------------+
| Bill |  Ted | arf arf | Excellent! |
|  bar |  pie |       ? |         hi |
|  baz | beam |    woof |         yo |
@@@@|]

    , testCase "medium sized table render, sorted" $
      cmpTables "medium table"
      (KTRA.render (cfg0 { KTR.sortKeyVals = True }) mediumKVI) [sq|
@@@@
| compiler | debug | optimization | Value |
+----------+-------+--------------+-------+
|     gcc7 |    no |            0 |   bad |
|     gcc7 |    no |            1 |  good |
|     gcc7 |   yes |            0 |  good |
|     gcc7 |   yes |            3 |  ugly |
|     gcc8 |   yes |            0 |  good |
|     gcc8 |   yes |            1 |   bad |
|     gcc8 |   yes |            3 |  true |
|   clang6 |   yes |            0 |    ok |
|   clang7 |    no |            0 |  good |
|   clang7 |    no |            1 |  good |
|   clang7 |    no |            3 |  good |
|   clang7 |   yes |            3 |  good |
|  clang10 |    no |            3 |  good |
|  clang10 |   yes |            3 |  good |
@@@@|]

    , testCase "medium sized table render, unsorted" $
      cmpTables "medium table"
      (KTRA.render (cfg0 { KTR.sortKeyVals = False }) mediumKVI) [sq|
@@@@
| compiler | debug | optimization | Value |
+----------+-------+--------------+-------+
|     gcc7 |   yes |            0 |  good |
|     gcc7 |   yes |            3 |  ugly |
|     gcc7 |    no |            0 |   bad |
|     gcc7 |    no |            1 |  good |
|     gcc8 |   yes |            0 |  good |
|     gcc8 |   yes |            1 |   bad |
|     gcc8 |   yes |            3 |  true |
|   clang6 |   yes |            0 |    ok |
|  clang10 |   yes |            3 |  good |
|  clang10 |    no |            3 |  good |
|   clang7 |   yes |            3 |  good |
|   clang7 |    no |            0 |  good |
|   clang7 |    no |            1 |  good |
|   clang7 |    no |            3 |  good |
@@@@|]

    , testCase "medium table, sorted, blank, row skip, no repeats" $
      cmpTables "medium table"
      (KTRA.render (cfg0 { KTR.sortKeyVals = True
                         , KTR.rowRepeat = False }) mediumKVI) [sq|
@@@@
| compiler | debug | optimization | Value |
+----------+-------+--------------+-------+
|     gcc7 |    no |            0 |   bad |
|          |       |            1 |  good |
|          |   yes |            0 |  good |
|          |       |            3 |  ugly |
|     gcc8 |   yes |            0 |  good |
|          |       |            1 |   bad |
|          |       |            3 |  true |
|   clang6 |   yes |            0 |    ok |
|   clang7 |    no |            0 |  good |
|          |       |            1 |  good |
|          |       |            3 |  good |
|          |   yes |            3 |  good |
|  clang10 |    no |            3 |  good |
|          |   yes |            3 |  good |
@@@@|]

    , testCase "medium table, sorted, blank row skip, no repeats, group unknown" $
      cmpTables "medium table s 0brow 0rep [unknown]"
      (KTRA.render (cfg0 { KTR.sortKeyVals = True
                         , KTR.rowRepeat = False
                         , KTR.rowGroup = ["unknown"]
                         }) mediumKVI) [sq|
@@@@
| compiler | debug | optimization | Value |
+----------+-------+--------------+-------+
|     gcc7 |    no |            0 |   bad |
|          |       |            1 |  good |
|          |   yes |            0 |  good |
|          |       |            3 |  ugly |
|     gcc8 |   yes |            0 |  good |
|          |       |            1 |   bad |
|          |       |            3 |  true |
|   clang6 |   yes |            0 |    ok |
|   clang7 |    no |            0 |  good |
|          |       |            1 |  good |
|          |       |            3 |  good |
|          |   yes |            3 |  good |
|  clang10 |    no |            3 |  good |
|          |   yes |            3 |  good |
@@@@|]

    , testCase "medium table, sorted, blank row skip, no repeats, group compiler" $
      cmpTables "medium table s 0brow 0rep [unknown]"
      (KTRA.render (cfg0 { KTR.sortKeyVals = True
                         , KTR.rowRepeat = False
                         , KTR.rowGroup = ["compiler"]
                         }) mediumKVI) [sq|
@@@@
| compiler | debug | optimization | Value |
+----------+-------+--------------+-------+
|     gcc7 |    no |            0 |   bad |
|          |       |            1 |  good |
|          |   yes |            0 |  good |
|          |       |            3 |  ugly |
+----------+-------+--------------+-------+
|     gcc8 |   yes |            0 |  good |
|          |       |            1 |   bad |
|          |       |            3 |  true |
+----------+-------+--------------+-------+
|   clang6 |   yes |            0 |    ok |
+----------+-------+--------------+-------+
|   clang7 |    no |            0 |  good |
|          |       |            1 |  good |
|          |       |            3 |  good |
|          |   yes |            3 |  good |
+----------+-------+--------------+-------+
|  clang10 |    no |            3 |  good |
|          |   yes |            3 |  good |
+----------+-------+--------------+-------+
@@@@|]

    , testCase "medium table, sorted, blank row skip, no repeats, multi-group" $
      cmpTables "medium table s 0brow 0rep [compiler,debug]"
      (KTRA.render (cfg0 { KTR.sortKeyVals = True
                         , KTR.rowRepeat = False
                         , KTR.rowGroup = ["unknown", "compiler", "unk", "debug", "huh"]
                         }) mediumKVI) [sq|
@@@@
| compiler | debug | optimization | Value |
+----------+-------+--------------+-------+
|     gcc7 |    no |            0 |   bad |
|          |       |            1 |  good |
|          |-------+--------------+-------+
|          |   yes |            0 |  good |
|          |       |            3 |  ugly |
+----------+-------+--------------+-------+
|     gcc8 |   yes |            0 |  good |
|          |       |            1 |   bad |
|          |       |            3 |  true |
+----------+-------+--------------+-------+
|   clang6 |   yes |            0 |    ok |
+----------+-------+--------------+-------+
|   clang7 |    no |            0 |  good |
|          |       |            1 |  good |
|          |       |            3 |  good |
|          |-------+--------------+-------+
|          |   yes |            3 |  good |
+----------+-------+--------------+-------+
|  clang10 |    no |            3 |  good |
|          |-------+--------------+-------+
|          |   yes |            3 |  good |
+----------+-------+--------------+-------+
@@@@|]

    ]
