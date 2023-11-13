--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)

import Data.Text.Lazy as T hiding (reverse)
import Data.Text.Lazy.Encoding as T
import Hakyll
import Hakyll.Process
import Prelude hiding (FilePath)
import Text.Pandoc.Options
import Turtle hiding (match)

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyllWith config $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match (fromList ["about.md"]) $ do
      route $ setExtension "html"
      compile
        $ pandocMdCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
    match "posts/*" $ do
      route $ setExtension "html"
      compile
        $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
    --create ["archive.html"] $ do
    --    route idRoute
    --    compile $ do
    --        posts <- recentFirst =<< loadAll "posts/*"
    --        let archiveCtx =
    --                listField "posts" postCtx (return posts) `mappend`
    --                constField "title" "Archives"            `mappend`
    --                defaultContext
    --        makeItem ""
    --            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --            >>= relativizeUrls
    match "index.html" $ do
      route idRoute
      compile $ do
        let indexCtx =
              (listField
                 "schedules"
                 (bodyField "schedule_body" `mappend` defaultContext)
                 (loadAll "schedules/*" >>= return . reverse))
                `mappend` defaultContext
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          -- >>= relativizeUrls
    match "schedules/*" $ do
      route $ setExtension "html"
      compile
        $ execCompilerWith
            (execName "./schedule_yaml2html.pl")
            [HakFilePath]
            CStdOut
            >>= return . fmap (T.unpack . T.decodeUtf8)
            >>= loadAndApplyTemplate "templates/schedule.html" defaultContext
            >>= relativizeUrls
            --(newExtOutFilePath "html")
      --route $ setExtension "html"
      --compile
      --  $ pandocMdCompiler
      --      >>= loadAndApplyTemplate "templates/schedule.html" defaultContext
      --      >>= relativizeUrls
    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
-- leftover form `hakyll-init`
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

pandocMdCompiler =
  pandocCompilerWith (def {readerExtensions = pandocExtensions}) def

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "docs" -- for github pages
    }
