{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AsciiRenderTests where

import           Control.Monad ( unless )
import           Data.Text ( Text )
import qualified Data.Text as T
import           Test.Tasty
import           Test.Tasty.HUnit

import           TestQQDefs

import qualified Data.KVITable as KVI
import qualified Data.KVITable.Render.ASCII as KTRA

cmpTables :: Text -> Text -> Text -> IO ()
cmpTables nm actual expected =
  unless (expected == actual) $ do
    let dl (e,a) = if e == a then db e else de " ↱" e <> "\n    " <> da " ↳" a
        db b = "|       > " <> b
        de m e = "|" <> m <> "expect> " <> e
        da m a = "|" <> m <> "actual> " <> a
        el = T.lines expected
        al = T.lines actual
        addnum n l = let nt = T.pack (show n)
                         nl = T.length nt
                     in T.take (4 - nl) "    " <> nt <> l
    let details = ("MISMATCH between expected and actual for " <> nm) :
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
  in
    [
      testCase "empty table" $
      cmpTables "empty table" (KTRA.render kvi0) [sq|
@@@@
| Value |
+-------+
|       |
@@@@
|]

    ]
