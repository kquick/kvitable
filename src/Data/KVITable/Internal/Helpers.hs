module Data.KVITable.Internal.Helpers where

import Data.Text ( Text )


type Keys = [Text]  -- Really [Keys], but Keys isn't defined yet


single :: e -> [e]
single = (:[])

snoc :: [a] -> a -> [a]
snoc l e = l <> [e]

maxOf :: (Foldable t, Ord e) => e -> t e -> e
maxOf v l = if null l then v else max v $ maximum l
