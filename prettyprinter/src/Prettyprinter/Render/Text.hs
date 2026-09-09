{-# LANGUAGE CPP               #-}

#include "version-compatibility-macros.h"

-- | Render an unannotated 'SimpleDocStream' as plain 'Text'.
module Prettyprinter.Render.Text (
#ifdef MIN_VERSION_text
    -- * Conversion to plain 'Text'
    renderLazy, renderStrict,
#endif

    -- * Render to a 'Handle'
    renderIO,

    -- ** Convenience functions
    putDoc, hPutDoc
) where



import           Data.Text              (Text)
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO      as TL
import           System.IO

import Prettyprinter
import Prettyprinter.Internal
import Prettyprinter.Render.Util.Panic

-- $setup
--
-- (Definitions for the doctests)
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text.IO as T
-- >>> import qualified Data.Text.Lazy.IO as TL



-- | @('renderLazy' sdoc)@ takes the output @sdoc@ from a rendering function
-- and transforms it to lazy text.
--
-- >>> let render = TL.putStrLn . renderLazy . layoutPretty defaultLayoutOptions
-- >>> let doc = "lorem" <+> align (vsep ["ipsum dolor", parens "foo bar", "sit amet"])
-- >>> render doc
-- lorem ipsum dolor
--       (foo bar)
--       sit amet
renderLazy :: SimpleDocStream ann -> TL.Text
renderLazy = TLB.toLazyText . go
  where
    go x = case x of
        SFail              -> panicUncaughtFail
        SEmpty             -> mempty
        SChar c rest       -> TLB.singleton c <> go rest
        SText _l t rest    -> TLB.fromText t <> go rest
        SLine i rest       -> TLB.singleton '\n' <> (TLB.fromText (textSpaces i) <> go rest)
        SAnnPush _ann rest -> go rest
        SAnnPop rest       -> go rest

-- | @('renderStrict' sdoc)@ takes the output @sdoc@ from a rendering function
-- and transforms it to strict text.
renderStrict :: SimpleDocStream ann -> Text
renderStrict = TL.toStrict . renderLazy



-- | @('renderIO' h sdoc)@ writes @sdoc@ to the file @h@.
--
-- >>> renderIO System.IO.stdout (layoutPretty defaultLayoutOptions "hello\nworld")
-- hello
-- world
--
-- The output is written in chunks as it is produced, so the whole document is
-- never held in memory at once:
--
-- @
-- 'renderIO' h sdoc = 'TL.hPutStr' h ('renderLazy' sdoc)
-- @
renderIO :: Handle -> SimpleDocStream ann -> IO ()
renderIO h sdoc = TL.hPutStr h (renderLazy sdoc)

-- | @('putDoc' doc)@ prettyprints document @doc@ to standard output. Uses the
-- 'defaultLayoutOptions'.
--
-- >>> putDoc ("hello" <+> "world")
-- hello world
--
-- @
-- 'putDoc' = 'hPutDoc' 'stdout'
-- @
putDoc :: Doc ann -> IO ()
putDoc = hPutDoc stdout

-- | Like 'putDoc', but instead of using 'stdout', print to a user-provided
-- handle, e.g. a file or a socket. Uses the 'defaultLayoutOptions'.
--
-- @
-- main = 'withFile' filename (\h -> 'hPutDoc' h doc)
--   where
--     doc = 'vcat' ["vertical", "text"]
--     filename = "someFile.txt"
-- @
--
-- @
-- 'hPutDoc' h doc = 'renderIO' h ('layoutPretty' 'defaultLayoutOptions' doc)
-- @
hPutDoc :: Handle -> Doc ann -> IO ()
hPutDoc h doc = renderIO h (layoutPretty defaultLayoutOptions doc)
