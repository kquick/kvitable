{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List ( sort )
import           Data.Maybe ( catMaybes )
import           Data.Text ( Text )
import qualified Data.Text as T
import           Lens.Micro ( (^.), (.~), (%~), (&) )
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.KVITable

import           Prelude hiding ( filter, lookup )
import           Debug.Trace


main :: IO ()
main = defaultMain $
       testGroup "kvitable tests" $
       let listing1 :: [ ( [(Key, KeyVal)], Text ) ]
           listing1 =
             [ ( [("foo", "bar"), ("moo", "cow")], "one" )
             , ( [("moo", "cow"), ("foo", "bar"), ("goose", "honk")], "two" )
             , ( [("moo", "cow")], "three" )
             , ( [("foo", "baz"), ("moo", "cow")], "four")
             ]
           kvi0 = mempty :: KVITable Text
           kvi1 = fromList listing1
           kvi1_1 = fromList $ take 2 listing1
           kvi1_2 = fromList $ drop 2 listing1

           mediumKVI = foldl foldlInsert
                       (mempty
                        & keyVals .~ [ ("compiler", [ "gcc7", "gcc8", "clang6", "clang10", "clang7" ])
                                     , ("debug", [ "yes", "no" ])
                                     , ("optimization", [ "0", "1", "3" ])
                                     ])
                       [ ([("compiler", "gcc7"), ("debug", "yes"), ("optimization", "0")], "good")
                       , ([("compiler", "gcc7"), ("debug", "no" ), ("optimization", "0")], "bad")
                       , ([("compiler", "gcc7"), ("debug", "yes"), ("optimization", "3")], "ugly")
                       , ([("compiler", "gcc8"), ("debug", "yes"), ("optimization", "0")], "good")
                       , ([("compiler", "clang6"), ("debug", "yes"), ("optimization", "0")], "ok")
                       , ([("compiler", "clang7"), ("debug", "no"), ("optimization", "1")], "good")
                       , ([("compiler", "clang7"), ("debug", "no"), ("optimization", "3")], "good")
                       , ([("compiler", "clang7"), ("debug", "yes"), ("optimization", "3")], "good")
                       , ([("compiler", "clang10"), ("debug", "no"), ("optimization", "3")], "good")
                       , ([("compiler", "clang10"), ("debug", "yes"), ("optimization", "3")], "good")
                       , ([("compiler", "gcc8"), ("debug", "yes"), ("optimization", "3")], "true")
                       , ([("compiler", "gcc8"), ("debug", "yes"), ("optimization", "1")], "bad")
                       , ([("compiler", "clang7"), ("debug", "no"), ("optimization", "0")], "good")
                       , ([("compiler", "gcc7"), ("debug", "no"), ("optimization", "1")], "good")
                       ]

           -- this table has floating values and keys with spaces
           ptable = foldl foldlInsert
                    (mempty & valueColName .~ "Annual Rainfall")
                    [ ([("City name", "Adelaide"), ("Area", "1295"), ("Population", "1158259")], 600.5)
                    , ([("City name", "Brisbane"), ("Area", "5905"), ("Population", "1857594")], 1146.4)
                    , ([("City name", "Darwin"), ("Area", "112"), ("Population", "120900")], 1714.7)
                    , ([("City name", "Hobart"), ("Area", "1357"), ("Population", "205556")], 619.5)
                    , ([("City name", "Melbourne"), ("Area", "1566"), ("Population", "3806092")], 646.9)
                    , ([("City name", "Perty"), ("Area", "5386"), ("Population", "1554769")], 869.4)
                    , ([("City name", "Sydney"), ("Area", "2058"), ("Population", "4336374")], 1214.8)
                    ]

           -- big and complicated
           zooTable = foldl foldlInsert
             (mempty & valueColName .~ "Count"
                     & keyVals .~ [ ("Location", ["San Diego", "LA", "Miami", "New York"])
                                  , ("Biome",    ["Savannah", "Jungle", "Polar"])
                                  , ("Category", ["Animal", "Reptile", "Bird"])
                                  , ("Diet",     ["Herbivore", "Carnivore"])
                                  , ("Name",     [])
                                  ])
             [ ([ ("Diet", "Carnivore")
                , ("Category", "Animal")
                , ("Biome", "Savannah")
                , ("Name", "Lion")
                , ("Location", "New York")], 3)
             , ([ ("Diet", "Carnivore")
                , ("Category", "Animal")
                , ("Biome", "Savannah")
                , ("Name", "Lion")
                , ("Location", "Miami")], 2)
             , ([ ("Diet", "Carnivore")
                , ("Category", "Animal")
                , ("Biome", "Savannah")
                , ("Name", "Lion")
                , ("Location", "LA")], 4)
             , ([ ("Diet", "Carnivore")
                , ("Category", "Animal")
                , ("Biome", "Savannah")
                , ("Name", "Lion")
                , ("Location", "San Diego")], 8)
             , ([ ("Location", "LA")
                , ("Biome", "Savannah")
                , ("Category", "Animal")
                , ("Name", "Giraffe")
                , ("Diet", "Herbivore")], 2)
             , ([ ("Location", "LA")
                , ("Biome", "Jungle")
                , ("Category", "Animal")
                , ("Name", "Hippo")
                , ("Diet", "Herbivore")], 1)
             , ([ ("Location", "LA")
                , ("Biome", "Savannah")
                , ("Category", "Animal")
                , ("Diet", "Herbivore")
                , ("Name", "Rhino")], 3)
             , ([ ("Location", "Miami")
                , ("Biome", "Polar")
                , ("Category", "Bird")
                , ("Diet", "Carnivore")
                , ("Subtype", "Gentoo")  -- new key
                , ("Name", "Penguin")], 20)
             , ([ ("Location", "San Diego")
                , ("Biome", "Polar")
                , ("Category", "Bird")
                , ("Diet", "Carnivore")
                , ("Subtype", "Emporer")
                , ("Name", "Penguin")], 8)
             , ([ ("Location", "San Diego")
                , ("Biome", "Polar")
                , ("Category", "Bird")
                , ("Diet", "Carnivore")
                , ("Subtype", "Gentoo")
                , ("Name", "Penguin")], 2)
             , ([ ("Location", "Miami")
                , ("Biome", "Savannah")
                , ("Category", "Animal")
                , ("Diet", "Herbivore")
                , ("Name", "Giraffe")
                , ("Subtype", "Reticulated")], 3)
             ]
           zooTable2 = insert [ ("Location", "San Diego"), ("Biome", "Plains")
                              , ("Category", "Animal"), ("Subtype", "Black")
                              , ("Name", "Bear"), ("Diet", "Omnivore") ] 1 $
                       insert [ ("Location", "San Diego"), ("Biome", "Plains")
                              , ("Category", "Animal"), ("Subtype", "Brown")
                              , ("Name", "Bear"), ("Diet", "Omnivore") ] 1 $
                       insert [ ("Location", "San Diego"), ("Biome", "Jungle")
                              , ("Category", "Animal"), ("Subtype", "Sun")
                              , ("Name", "Bear"), ("Diet", "Omnivore") ] 1 $
                       insert [ ("Location", "San Diego"), ("Biome", "Polar")
                              , ("Category", "Animal"), ("Subtype", "Polar")
                              , ("Name", "Bear"), ("Diet", "Omnivore") ] 1 $
                       adjust succ ([ ("Category", "Animal")
                                    , ("Diet", "Carnivore")
                                    , ("Biome", "Savannah")
                                    , ("Location", "San Diego")
                                    , ("Name", "Lion")])
                       zooTable

           testedTable = foldl foldlInsert
                         (mempty
                          & keyVals .~ [ ("system",   ["x86_64-linux", "x86_64-darwin"])
                                       , ("Branch",   ["master", "develop", "PR-feature"])
                                       , ("Strategy", ["submodules", "HEADs"])
                                       , ("ghcver",   ["ghc844", "ghc865", "ghc882", "ghc890"])
                                       , ("debug",    ["Y", "N"])
                                       ])
                         $ let ls g b d v = ([ ("system", "x86_64-linux")
                                             , ("Strategy", "submodules")
                                             , ("ghcver", g), ("Branch", b)
                                             , ("debug", d) ], v)
                               lH g b d v = ([ ("system", "x86_64-linux")
                                             , ("Strategy", "HEADs")
                                             , ("ghcver", g), ("Branch", b)
                                             , ("debug", d) ], v)
                               ms g b d v = ([ ("system", "x86_64-darwin")
                                             , ("Strategy", "submodules")
                                             , ("ghcver", g), ("Branch", b)
                                             , ("debug", d) ], v)
                               mH g b d v = ([ ("system", "x86_64-darwin")
                                             , ("Strategy", "HEADs")
                                             , ("ghcver", g), ("Branch", b)
                                             , ("debug", d) ], v)
                           in [ ls "ghc844" "PR-feature" "Y" "+"
                              , ls "ghc844" "PR-feature" "N" "+"
                              , lH "ghc865" "PR-feature" "N" "FAIL*2"
                              , ls "ghc882" "develop" "N" "FAIL*1"
                              , lH "ghc865" "develop" "N" "+"
                              , ls "ghc844" "master" "Y" "FAIL*1"
                              , ls "ghc844" "master" "N" "+"
                              , lH "ghc844" "master" "Y" "FAIL*1"
                              , lH "ghc865" "master" "N" "FAIL*1"
                              , lH "ghc882" "master" "N" "FAIL*1"
                              , ls "ghc865" "master" "N" "FAIL*1"
                              , lH "ghc844" "develop" "Y" "+"
                              , lH "ghc844" "develop" "N" "+"
                              , lH "ghc882" "PR-feature" "N" "FAIL*1"
                              , ls "ghc882" "PR-feature" "N" "FAIL*1"
                              , ls "ghc865" "PR-feature" "N" "+"
                              , ls "ghc844" "develop" "N" "+"
                              , ls "ghc844" "develop" "Y" "+"
                              , mH "ghc844" "develop" "N" "+"
                              ]

           nestedTable = foldl foldlInsert
                         (mempty
                          & keyVals .~ [ ("millions",  ["0"])
                                       , ("thousands", ["0"])
                                       , ("hundreds",  ["0"])
                                       , ("tens",      ["0"])
                                       , ("ones",      ["0"])
                                       ])
                         [ ([("millions", T.pack $ show m)
                            ,("thousands", T.pack $ show t)
                            ,("hundreds", T.pack $ show h)
                            ,("tens", T.pack $ show d)
                            ,("ones", T.pack $ show o)],
                             if (o `rem` 1) == 1 then "odd" else "even")
                         | m <- [0..3 :: Int]
                         , t <- [0..3 :: Int]
                         , h <- [1..3 :: Int]
                         , d <- [2..3 :: Int]
                         , o <- [0..2 :: Int]
                         ]

       in
         [
           testCase "empty table" $ (mempty :: KVITable Bool) @=? (mempty :: KVITable Bool)

         , testCase "to and from" $ length listing1 @=? length (toList kvi1)

         , testCase "length" $ length listing1 @=? length kvi1

         , testCase "mappable" $
           sum (snd <$> (fmap T.length) <$> listing1) @=?
           sum (snd <$> toList (T.length <$> kvi1))

         , testCase "traversable" $
           Just (sort $ snd <$> listing1) @=?
           sort . fmap snd . toList <$> (traverse Just kvi1)

         , testCaseSteps "semigroup" $ \step ->
             do step "empty to full"
                mempty <> kvi1 @?= kvi1
                step "full to empty"
                kvi1 <> mempty @?= kvi1
                step "idempotent"
                kvi1 <> kvi1 @?= kvi1

                step "join parts"
                kvi1_1 <> kvi1_2 @?= kvi1  -- happens to be the right order

                -- note: it is *not* the case that
                --
                -- >   kvi1_2 <> kvi1_1 == kvi1
                --
                -- because the keyvals from a semigroup operation are
                -- built dynamically and this particular kvi1_1 /
                -- kvi1_2 will result in a different key order in
                -- reverse.  However, specifying the keyVals
                -- explicitly can eliminate key detection ordering concerns.

                step "keyed"
                let keyed = mempty & keyVals .~ [ ("foo", ["baz", "", "bar"])
                                                , ("moo", ["cow"])
                                                , ("goose", ["honk", ""])
                                                ]
                keyed <> kvi1_1 <> kvi1_2 @?= kvi1

         , testCase "empty lookup fails" $
           Nothing @=? lookup [("foo", "bar"), ("moo", "cow")] (mempty :: KVITable Bool)

         , testCase "regular lookup" $
           Just "one" @=? lookup [("foo", "bar"), ("moo", "cow")] kvi1

         , testCase "add to empty table" $
           let keyvals = [ ("foo", "bar"), ("moo", "cow") ]
           in [ (keyvals, "hi") ] @=? toList (insert keyvals "hi" kvi0)

         , testCase "getRows on empty table" $ [] @=? rows kvi0

         , testCase "rows in simple table" $
           let keyvals = [ ("foo", "bar"), ("moo", "cow") ]
           in [ (["bar", "cow"], "hi") ] @=? rows (insert keyvals "hi" kvi0)

         , testCaseSteps "non-leaf kvitable insert uses default key value" $ \step ->
             do let keyvals = [ ("foo", "bar"), ("moo", "cow") ]
                    t1 = insert keyvals "hi" kvi0

                step "at start"
                let t2 = insert [ ("moo", "dog") ] "oops" t1
                rows t2 @?= [ ([ "",    "dog" ], "oops")
                            , ([ "bar", "cow" ], "hi")
                            ]

                step "at end"
                let t3 = insert [ ("foo", "dog") ] "oops" t1
                rows t3 @?= [ ([ "bar", "cow" ], "hi")
                            , ([ "dog", ""    ], "oops")
                            ]

                step "in middle"
                let keyvals' = [ ("foo", "bar"), ("moo", "cow"), ("oink", "pig") ]
                    t1' = insert keyvals' "hi" kvi0
                    t4 = insert [ ("foo", "baz"), ("oink", "hog") ] "oops" t1'
                rows t4 @?= [ ([ "bar", "cow", "pig" ], "hi")
                            , ([ "baz", "",    "hog" ], "oops")
                            ]

         , testCaseSteps "valueColName" $ \step ->
             do step "fetch"
                "Value" @=? kvi1 ^. valueColName
                step "set"
                "says" @=? (kvi1 & valueColName .~ "says") ^. valueColName
                step "update"
                "says Value" @=? (kvi1 & valueColName %~ ("says " <>)) ^. valueColName

         , testCase "keyVals fetch" $ [ ("foo", ["", "bar", "baz"])
                                      , ("moo", ["cow"])
                                      , ("goose", ["", "honk"])
                                      ] @=? kvi1 ^. keyVals

         , testCaseSteps "lookup" $ \step ->
             do step "valid #1"
                Just "three" @=? lookup [("moo", "cow")] kvi1
                step "valid #2"
                Just "two"   @=? lookup [("goose", "honk"), ("moo", "cow"), ("foo", "bar")] kvi1
                step "valid #3"
                Just "four"  @=? lookup [("moo", "cow"), ("foo", "baz")] kvi1
                step "valid #4"
                Just "one"   @=? lookup [("moo", "cow"), ("foo", "bar")] kvi1
                step "valid with dups"
                -- Note: ok to duplicate key values with identical entries
                Just "one"   @=? lookup [("moo", "cow"), ("foo", "bar"), ("moo", "cow")] kvi1
                step "invalid #1"
                Nothing @=? lookup [("moo", "bar")] kvi1
                step "invalid #2"
                Nothing @=? lookup [("foo", "moo"), ("cow", "moo")] kvi1
                step "invalid #3"
                Nothing @=? lookup [] kvi1

         , testCase "multiply-specified lookups" $
             -- one of these will work, one will fail, but it's
             -- indeterminate which one.  This is not a recommended
             -- usage, but rather than waste computational resources
             -- to prevent it, this is simply documenting this as a
             -- known behavior and users are discouraged from using
             -- it.
             1 @=? (length $ catMaybes [ lookup [("moo", "sheep"), ("foo", "bar"), ("moo", "cow")] kvi1
                                       , lookup [("moo", "cow"), ("foo", "bar"), ("moo", "sheep")] kvi1
                                       ])

         , testCase "deep add" $
           let t0 = mempty
                    & keyVals .~ [ ("foo", ["bar", "baz"])
                                 , ("moon", ["beam", "pie"])
                                 ]
                    & valueColName .~ "says"
               t1 = insert [ ("foo", "Bill"), ("moon", "Ted"), ("dog", "arf arf") ] "Excellent!" $
                    insert [ ("foo", "baz"), ("moon", "beam"), ("dog", "woof") ] "yo" $
                    insert [ ("foo", "bar"), ("moon", "pie") ] "hi"
                    t0
           in rows t1 @?= [ ([ "Bill", "Ted", "arf arf" ], "Excellent!")
                          , ([ "bar", "pie", "" ], "hi")
                          , ([ "baz", "beam", "woof"], "yo")
                          ]

         , testCase "medium sized table rows" $
           rows mediumKVI @?=
           [ ([ "clang10", "no", "3"], "good" )
           , ([ "clang10", "yes", "3"], "good" )
           , ([ "clang6", "yes", "0"], "ok" )
           , ([ "clang7", "no", "0"], "good" )
           , ([ "clang7", "no", "1"], "good" )
           , ([ "clang7", "no", "3"], "good" )
           , ([ "clang7", "yes", "3"], "good" )
           , ([ "gcc7", "no", "0"], "bad" )
           , ([ "gcc7", "no", "1"], "good" )
           , ([ "gcc7", "yes", "0"], "good" )
           , ([ "gcc7", "yes", "3"], "ugly" )
           , ([ "gcc8", "yes", "0"], "good" )
           , ([ "gcc8", "yes", "1"], "bad" )
           , ([ "gcc8", "yes", "3"], "true" )
           ]

         , testCase "filter" $
           rows (filter (\(spec,val) -> ("compiler", "gcc7") `elem` spec) mediumKVI) @?=
           [ ([ "gcc7", "no", "0"], "bad" )
           , ([ "gcc7", "no", "1"], "good" )
           , ([ "gcc7", "yes", "0"], "good" )
           , ([ "gcc7", "yes", "3"], "ugly" )
           ]

         , testCaseSteps "zoo contents" $ \step -> do
             step "LA Lions"
             Just 4 @=? lookup [ ("Location", "LA"), ("Name", "Lion")
                               , ("Diet", "Carnivore"), ("Category", "Animal")
                               , ("Biome", "Savannah"), ("Subtype", "")
                               ] zooTable2
             step "No polar lions"
             Nothing @=? lookup [ ("Location", "LA"), ("Name", "Lion")
                                , ("Diet", "Carnivore"), ("Category", "Animal")
                                , ("Biome", "Polar"), ("Subtype", "")
                                ] zooTable2
         ]
