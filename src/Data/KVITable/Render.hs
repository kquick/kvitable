-- | Common definitions (and support functions) for rendering a
-- 'KVITable'.

module Data.KVITable.Render
  (
    RenderConfig(..)
  , defaultRenderConfig
  )
where

import           Data.Text ( Text )

import           Data.KVITable


-- | Returns the default rendering configuration, to be used with a
-- format-specific @render@ call.

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

-- | The 'RenderConfig' specifies the various controls and
-- configurations used when rendering a 'KVITable' in various formats.
-- The 'RenderConfig' is global t oall formats, although some of the
-- fields in the 'RenderConfig' will be ignored as not-applicable by
-- some formats.

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
    -- ^ 'True' (default) if an identical 'KeyVal' is to be repeated
    -- in subsequent applicable rows.

  , rowGroup :: [Key]
    -- ^ List of Key names that should by grouped by inserting
    -- horizontal row lines between KeyVals

  , caption :: Maybe Text
    -- ^ Caption to render for table for backends which support
    -- captions; otherwise ignored.
  }



