{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Data.KVITable

main :: IO ()
main = defaultMain $
       testGroup "kvitable tests"
       [
         testCase "empty table" $ kvitable @=? "your table here"
       ]
