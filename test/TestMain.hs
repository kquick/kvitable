{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List ( sort )
import           Data.Text ( Text )
import qualified Data.Text as T
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.KVITable

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
           kvi1 = fromList listing1
       in
         [
           testCase "empty table" $ (mempty :: KVITable Bool) @=? (mempty :: KVITable Bool)

         , testCase "to and from" $ sort listing1 @=? sort (toList kvi1)

         , testCase "length" $ length listing1 @=? length kvi1

         , testCase "mappable" $ sort ((fmap T.length) <$> listing1) @=? sort (toList (T.length <$> kvi1))

         , testCase "traversable" $ Just (sort listing1) @=? sort . toList <$> (traverse Just kvi1)

         ]
