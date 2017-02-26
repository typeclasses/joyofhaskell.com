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

archiveCtx posts =
  listField "posts" postCtx (return posts)
  <> constField "title" "Archives"
  <> defaultContext

indexCtx posts =
  listField "posts" postCtx (return posts)
  <> constField "title" "Home"
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
    route $ setExtension "html"
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/default.html"    postCtx
      >>= relativizeUrls

posts :: Rules ()
posts = do
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ compiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= relativizeUrls

archive :: Rules ()
archive = do
  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" (archiveCtx posts)
        >>= loadAndApplyTemplate "templates/default.html" (archiveCtx posts)
        >>= relativizeUrls

index :: Rules ()
index = do
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      getResourceBody
        >>= applyAsTemplate (indexCtx posts)
        >>= relativizeUrls

templates :: Rules ()
templates = match "templates/*" $ compile templateCompiler

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
  archive
  index
  templates
