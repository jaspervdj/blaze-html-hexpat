-- | This is a module which runs the 'HtmlBenchmarks' module using the different
-- renderers available.
--
{-# LANGUAGE OverloadedStrings #-}
module RunHtmlBenchmarks where

import Criterion.Main
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LB

import Text.Blaze (Html)
import qualified Text.Blaze.Renderer.Hexpat as Hexpat
import Text.XML.Expat.Format (format)
import qualified Text.XML.Expat.Tree as X

import HtmlBenchmarks (HtmlBenchmark (..), benchmarks)

-- | Function to run the benchmarks using criterion
--
main :: IO ()
main = defaultMain $ concatMap benchHtml benchmarks
  where
    benchHtml (HtmlBenchmark name f x _) =
        [ bench (name ++ " (Hexpat)")   $ nf (LB.length .  render . f) x
        ]

render :: Html -> LB.ByteString
render = format . X.Element "html" [] . Hexpat.renderHtml
