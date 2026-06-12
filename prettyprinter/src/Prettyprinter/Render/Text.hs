{-# LANGUAGE BangPatterns      #-}
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
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
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
renderLazy = TLB.toLazyText . go 0
  where
    -- The first argument is indentation that is only printed once the line
    -- turns out to be non-blank.
    -- See Note [Deferred indentation of blank lines] in Prettyprinter.Internal.
    go !pending x = case x of
        SFail              -> panicUncaughtFail
        SEmpty             -> mempty
        SChar c rest       -> indentation <> TLB.singleton c <> go 0 rest
        SText _l t rest    -> indentation <> TLB.fromText t <> go 0 rest
        SLine i rest       -> TLB.singleton '\n' <> go i rest
        SAnnPush _ann rest -> indentation <> go 0 rest
        SAnnPop rest       -> indentation <> go 0 rest
      where
        indentation = TLB.fromText (textSpaces pending)

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
-- This function is more efficient than @'T.hPutStr' h ('renderStrict' sdoc)@,
-- since it writes to the handle directly, skipping the intermediate 'Text'
-- representation.
renderIO :: Handle -> SimpleDocStream ann -> IO ()
renderIO h = go 0
  where
    -- The first argument is indentation that is only printed once the line
    -- turns out to be non-blank.
    -- See Note [Deferred indentation of blank lines] in Prettyprinter.Internal.
    go :: Int -> SimpleDocStream ann -> IO ()
    go !pending = \sds -> case sds of
        SFail              -> panicUncaughtFail
        SEmpty             -> pure ()
        SChar c rest       -> do indentation
                                 hPutChar h c
                                 go 0 rest
        SText _ t rest     -> do indentation
                                 T.hPutStr h t
                                 go 0 rest
        SLine n rest       -> do hPutChar h '\n'
                                 go n rest
        SAnnPush _ann rest -> do indentation
                                 go 0 rest
        SAnnPop rest       -> do indentation
                                 go 0 rest
      where
        indentation = case pending of
            0 -> pure ()
            n -> T.hPutStr h (textSpaces n)

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
