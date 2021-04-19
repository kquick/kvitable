module Data.KVITable.Render
  (
    RenderConfig(..)
  , defaultRenderConfig
  )
where

import Data.KVITable
import Data.Text ( Text )


defaultRenderConfig :: RenderConfig
defaultRenderConfig = RenderConfig
  { hideBlankRows = True
  , hideBlankCols = True
  , equisizedCols = True
  , sortKeyVals   = False
  , colStackAt    = Nothing
  , rowRepeat     = True
  , rowGroup      = []
  , caption       = Nothing
  }

data RenderConfig = RenderConfig
  {
    hideBlankRows :: Bool
    -- ^ 'True' (default) removes rows for which there are no values

  , hideBlankCols :: Bool
    -- ^ 'True' (default) removes columns for which there are no values

  , equisizedCols :: Bool
    -- ^ 'True' (default) to maintain a consistent column width,
    -- otherwise the columns are shunk to the minimum size needed to
    -- display the title and values.  Not applicable for some backends
    -- (e.g. HTML) where the backend provides table rendering
    -- functionality.

  , sortKeyVals :: Bool
    -- ^ 'True' (default is False) to sort the KeyVal entries when
    -- rendering a table.

  , colStackAt :: Maybe Key
    -- ^ Column key to begin stacking keys in columns and sub-columns
    -- rather than creating additional sub-rows.

  , rowRepeat :: Bool
    -- ^ 'True' (default) if an identical entry is to be repeated in subsequent rows  [?? KWQ: clarify ...]

  , rowGroup :: [Key]
    -- ^ List of Key names that should by grouped by inserting
    -- horizontal row lines between KeyVals

  , caption :: Maybe Text
    -- ^ Caption to render for table for backends which support
    -- captions; otherwise ignored.
  }
