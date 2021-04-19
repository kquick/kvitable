{-# LANGUAGE OverloadedStrings #-}

module Data.KVITable.Render.ASCII
  (
    render
  )
where

import Data.KVITable
import Data.Text ( Text )
import Prettyprinter


render :: Pretty v => KVITable v -> Text
render t = "table rendered"
