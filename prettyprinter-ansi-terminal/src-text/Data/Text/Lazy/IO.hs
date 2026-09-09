-- Provide a fake API, mimicking Data.Text.Lazy.IO from text package,
-- but actually backed by type Text = String. It is used only in rare
-- circumstances, when prettyprinter is built with -text flag.
--

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Text.Lazy.IO where

import qualified System.IO

hPutStr = System.IO.hPutStr
