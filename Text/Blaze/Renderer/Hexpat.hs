-- | Renderer that supports rendering to expat forests
--
module Text.Blaze.Renderer.Hexpat
    ( Forest
    , renderHtml
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Text.XML.Expat.Tree as X

import Text.Blaze.Internal

-- | Type used as list of nodes
--
type Forest = [X.Node ByteString ByteString]

-- | Render a 'ChoiceString' to a bytestring. This is only meant to be used for
-- shorter strings, since it is inefficient for large strings.
--
fromChoiceStringSB :: ChoiceString -> ByteString
fromChoiceStringSB (Static s)               = getUtf8ByteString s
fromChoiceStringSB (String s)               = T.encodeUtf8 $ T.pack s
fromChoiceStringSB (Text s)                 = T.encodeUtf8 s
fromChoiceStringSB (ByteString s)           = s
fromChoiceStringSB (PreEscaped s)           = fromChoiceStringSB s
fromChoiceStringSB (External s)             = fromChoiceStringSB s
fromChoiceStringSB (AppendChoiceString x y) =
    fromChoiceStringSB x `SB.append` fromChoiceStringSB y
fromChoiceStringSB EmptyChoiceString        = SB.empty
{-# INLINE fromChoiceStringSB #-}

-- | Render a 'ChoiceString' to an appending list of nodes
--
fromChoiceString :: ChoiceString
                 -> Forest
                 -> Forest
fromChoiceString s@(Static _)     = (X.Text (fromChoiceStringSB s) :)
fromChoiceString s@(String _)     = (X.Text (fromChoiceStringSB s) :)
fromChoiceString s@(Text _)       = (X.Text (fromChoiceStringSB s) :)
fromChoiceString s@(ByteString _) = (X.Text (fromChoiceStringSB s) :)
fromChoiceString (PreEscaped s)   =
    -- TODO: Research options for inserting pre-escaped text into expat
    -- (possibly decoding?)
    fromChoiceString s
fromChoiceString (External s)     =
    -- TODO: Inspect external nodes
    fromChoiceString s
fromChoiceString (AppendChoiceString x y) =
    fromChoiceString x . fromChoiceString y
fromChoiceString EmptyChoiceString = id
{-# INLINE fromChoiceString #-}

-- | Render some 'Html' to an appending list of nodes
--
renderNodes :: Html
            -> Forest
            -> Forest
renderNodes = go []
  where
    go :: [(ByteString, ByteString)] -> HtmlM b -> Forest -> Forest
    go attrs (Parent tag _ _ content) =
        (X.Element (getUtf8ByteString tag) attrs (go [] content []) :)
    go attrs (Leaf tag _ _) =
        (X.Element (getUtf8ByteString tag) attrs [] :)
    go attrs (AddAttribute key _ value content) =
        go ((getUtf8ByteString key, fromChoiceStringSB value) : attrs) content
    go attrs (AddCustomAttribute key _ value content) =
        go ((fromChoiceStringSB key, fromChoiceStringSB value) : attrs) content
    go _ (Content content) = fromChoiceString content
    go attrs (Append h1 h2) = go attrs h1 . go attrs h2
    go _ Empty = id
    {-# NOINLINE go #-}
{-# INLINE renderNodes #-}

-- | Render HTML to an expat forest
--
renderHtml :: Html -> Forest
renderHtml html = renderNodes html []
{-# INLINE renderHtml #-}
