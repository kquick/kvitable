{-# LANGUAGE DataKinds #-}

-- | Common definitions (and support functions) for rendering a
-- 'KVITable'.

module Data.KVITable.Render
  (
    RenderConfig(..)
  , defaultRenderConfig
  )
where

import Data.Name ( Name )
import Numeric.Natural

import Data.KVITable


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
  , maxCells      = 40000 -- even this is probably too big
  , maxCols       = 40000 -- by default, this should be >= maxCells
  }

-- | The 'RenderConfig' specifies the various controls and
-- configurations used when rendering a 'KVITable' in various formats.
-- The 'RenderConfig' is global to all formats, although some of the
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

  , caption :: Maybe (Name "caption")
    -- ^ Caption to render for table for backends which support
    -- captions; otherwise ignored.

  , maxCells :: Natural
    -- ^ The maximum number of cells that will be rendered.  The size of the
    -- table is #rows times #cols, so a 100x100 table is 10000 entries. This
    -- value is used to limit the size of the rendered table to provide a
    -- reasonable output in a reasonable amount of time and memory; tables larger
    -- than this limit will return a "table too big" message when rendered.
    --
    -- ASCII: At least 1 character for the cell, plus 3.5 characters for
    --  boundaries means that even on an ultra-wide monitor with a very small
    --  font, the table isn't very readable beyond 30-40 columns.  In addition,
    --  the algorithm needs to continually adjust table column widths to
    --  accomodate new values, so there is a great deal of backtracking involved
    --  and the time taken to render grows quite quickly.
    --
    -- HTML: A browser has reasonable constraints on displaying a table: 10000
    --  entries is difficult for the user to comprehend, but the browser is
    --  probably reasonably performant.  At 500x500, the browser is likely to be
    --  very sluggish, with visible delays in rendering visible regions during
    --  scrolling.

  , maxCols :: Natural
    -- ^ The maximum number of columns to render.  This limit is only useful if
    -- it is set to less than the 'maxCells' value, and it is useful in that case
    -- to ensure that more than one (partial) row is displayed.  The 'maxCells'
    -- value takes priority over this value.  See the 'maxCells' for more
    -- information.
  }
