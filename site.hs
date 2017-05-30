{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Hakyll
import Text.Pandoc
import Data.Maybe (catMaybes)
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
  <> constField "title" "Blog - The Joy of Haskell"
  <> constField "twitter-title" "The Joy of Haskell - Blog"
  <> constField "twitter-description" "Updates about the book progress and other Haskell-related writings"
  <> defaultContext

authorsCtx =
  constField "title" "Authors - The Joy of Haskell"
  <> constField "twitter-title" "The Joy of Haskell - Authors"
  <> constField "twitter-description" "Julie Moronuki & Chris Martin"
  <> defaultContext

indexCtx =
  constField "title" "The Joy of Haskell"
  <> constField "twitter-title" "The Joy of Haskell"
  <> constField "twitter-description" "A complete guide to the Haskell ecosystem. For intermediate to advanced Haskellers. "
  <> constField "twitter-url" "/"
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

posts :: Rules ()
posts = do
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ compiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

blog :: Rules ()
blog = do
  create ["blog.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< filterListed =<< loadAll "posts/*"
      makeItem ""
        >>= loadAndApplyTemplate "templates/blog.html" (blogCtx posts)
        >>= loadAndApplyTemplate "templates/default.html" (blogCtx posts)
        >>= relativizeUrls

authors :: Rules ()
authors = do
  create ["authors.html"] $ do
    route idRoute
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/authors.html" authorsCtx
        >>= loadAndApplyTemplate "templates/default.html" authorsCtx
        >>= relativizeUrls

rssFeed :: Rules ()
rssFeed = do
  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      posts <- fmap (take 10) . recentFirst =<< filterListed =<<
            loadAllSnapshots "posts/*" "content"
      renderRss feedConfig (bodyField "description" <> postCtx) posts

filterListed :: MonadMetadata m => [Item a] -> m [Item a]
filterListed =
    filterM $ isListed . itemIdentifier
  where
    filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
    filterM f xs =
      catMaybes <$> traverse (\i -> (\b -> if b then Just i else Nothing) <$> f i) xs

isListed :: forall m.
  MonadMetadata m
  => Identifier -- ^ Input page
  -> m Bool
isListed id' =
  getMetadata id' >>= \(metadata :: Metadata) ->
  case lookupString "listed" metadata of
    Nothing -> fail $ "There is no 'listed' attribute for " <> show id'
    Just s -> parseListed s

  where
    parseListed :: String -> m Bool
    parseListed "true" = pure True
    parseListed "false" = pure False
    parseListed s = fail $ "Could not parse 'listed' attribute for " <> show id' <> ": \"" <> s <> "\""

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

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
     { feedTitle       = "The Joy of Haskell"
     , feedDescription = "Posts about Haskell and book progress."
     , feedAuthorName  = "Julie Moronuki and Chris Martin"
     , feedAuthorEmail = "hello@joyofhaskell.com"
     , feedRoot        = "https://joyofhaskell.com"
     }

main :: IO ()
main = hakyllWith cfg $ do
  static
  authors
  posts
  blog
  index
  templates
  code
  rssFeed
