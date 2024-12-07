--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)

import Data.Bifunctor
import Data.ByteString.Char8 as B (ByteString, pack)
import Data.Function
import Data.List
import Data.Maybe
import Data.String
import Data.Text.Lazy as T hiding (reverse)
import Data.Text.Lazy.Encoding as T
import Hakyll
import Hakyll.Process
import Prelude hiding (FilePath)
import Text.Pandoc.Options
import qualified Text.Regex.PCRE.Light as PCRE

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
    --
    --            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --            >>= relativizeUrls
    let collate_schedules =
          \dir -> do
            route idRoute
            compile $ do
              let indexCtx =
                    let comparator (x :: Identifier) (y :: Identifier) =
                          let re :: PCRE.Regex =
                                PCRE.compile
                                  ("(\\d)+-(fall|winter|spring-summer)")
                                  [] --[PCRE.caseless]
                           in fromJust $ do
                                xmatches :: [ByteString] <-
                                  PCRE.match re (B.pack . show $ x) []
                                ymatches :: [ByteString] <-
                                  PCRE.match re (B.pack . show $ y) []
                                -- HACK: fall, spring-summer, winter is lexicographical order, but want winter, spring-summer, fall
                                let make_into_lexicographic_order semester
                                      | semester == "fall" = "xaa"
                                      | semester == "winter" = "xab"
                                      | semester == "spring-summer" = "xac"
                                return
                                  $ case (xmatches !! 1)
                                           `compare` (ymatches !! 1) of
                                      EQ ->
                                        let (d, d') =
                                              bimap
                                                make_into_lexicographic_order
                                                make_into_lexicographic_order
                                                (xmatches !! 2, ymatches !! 2)
                                         in d `compare` d'
                                      c -> c
                     in (listField
                           dir
                           (bodyField "schedule_body" `mappend` defaultContext)
                           (loadAll (fromString $ dir ++ "/*")
                              >>= return
                                    . reverse
                                    . (sortBy (comparator `on` itemIdentifier))))
                          `mappend` defaultContext
              getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
    match "index.html" $ collate_schedules "current_schedules"
    match "past.html" $ collate_schedules "past_schedules"
    let yaml2html shouldReverse = do
          route $ setExtension "html"
          compile
            $ execCompilerWith
                (execName "./schedule_yaml2html.pl")
                [HakFilePath, ProcArg shouldReverse]
                CStdOut
                >>= return . fmap (T.unpack . T.decodeUtf8)
                >>= loadAndApplyTemplate
                      "templates/schedule.html"
                      defaultContext
                >>= relativizeUrls
    match "current_schedules/*" (yaml2html "1")
            --(newExtOutFilePath "html")
      --route $ setExtension "html"
      --compile
      --  $ pandocMdCompiler
      --      >>= loadAndApplyTemplate "templates/schedule.html" defaultContext
      --      >>= relativizeUrls
    match "past_schedules/*" (yaml2html "0")
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
