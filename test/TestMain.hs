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

import           Prelude hiding ( lookup )
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
                rows t2 @?= [ ([ "bar", "cow" ], "hi")
                            , ([ "",    "dog" ], "oops")
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

         , testCase "keyVals fetch" $ [ ("foo", ["baz", "", "bar"])
                                      , ("moo", ["cow"])
                                      , ("goose", ["honk", ""])
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
         ]
