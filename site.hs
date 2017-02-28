{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Main where

import Hakyll
import Text.Pandoc
import Data.Monoid ((<>))
import qualified Data.Map as M
import System.FilePath (joinPath, splitPath, replaceExtension)

--------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
  <> defaultContext

-- mathCtx :: Context String
-- mathCtx = field "mathjax" $ \item -> do
--   metadata <- getMetadata $ itemIdentifier item
--   return $ if "mathjax" `M.member` metadata
--            then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
--            else ""

blogCtx posts =
  listField "posts" postCtx (return posts)
  <> constField "title" "Blog"
  <> defaultContext

indexCtx =
  constField "title" "Home"
  <> defaultContext

--------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------

static :: Rules ()
static = do
  match "images/*" $ do
    route idRoute
    compile $ copyFileCompiler
  match "css/partials/*.scss" (compile getResourceBody)
  cssDeps <- makePatternDependency "css/partials/*.scss"
  rulesExtraDependencies [cssDeps] $
    match "css/*.scss" $ do
      route (setExtension "css")
      compile scssCompiler
  -- match "js/*" $ do
  --   route idRoute
  --   compile $ copyFileCompiler

scssCompiler :: Compiler (Item String)
scssCompiler = do
    input <- getResourceFilePath
    output <- unixFilter "sassc" [input] ""
    makeItem output

pages :: Rules ()
pages = do
  match "pages/*" $ do
    route $ customRoute $ joinPath . tail . splitPath
                        . replaceExtension "html"
                        . toFilePath
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/default.html"    postCtx
      >>= relativizeUrls

posts :: Rules ()
posts = do
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ compiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

blog :: Rules ()
blog = do
  create ["blog.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      makeItem ""
        >>= loadAndApplyTemplate "templates/blog.html" (blogCtx posts)
        >>= loadAndApplyTemplate "templates/default.html" (blogCtx posts)
        >>= relativizeUrls

index :: Rules ()
index = do
  match "index.html" $ do
    route idRoute
    compile $ do
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= relativizeUrls

templates :: Rules ()
templates = match "templates/*" $ compile templateCompiler

code :: Rules ()
code = do
  match "code/*" $ do
    route (setExtension "html")
    compile pandocCompiler


--------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------

compiler :: Compiler (Item String)
compiler = pandocCompilerWith defaultHakyllReaderOptions pandocOptions

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }

cfg :: Configuration
cfg = defaultConfiguration

main :: IO ()
main = hakyllWith cfg $ do
  static
  pages
  posts
  blog
  index
  templates
  code
