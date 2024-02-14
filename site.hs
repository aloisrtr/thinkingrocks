--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss, lookupHighlightingStyle)
import Text.Pandoc.Class (PandocIO, runIOorExplode)
import Text.Pandoc.Options      (ReaderOptions (..), WriterOptions (..))
import Text.Pandoc.Error (PandocError)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration { destinationDirectory = "docs" }

pandocCodeStyle :: IO Style
pandocCodeStyle = runIOorExplode (lookupHighlightingStyle "flexoki.theme")

rssFeedConfiguration :: FeedConfiguration
rssFeedConfiguration = FeedConfiguration {
    feedTitle = "ThinkingRocks",
    feedDescription = "Blogposts about the endeavours of a computer science student",
    feedAuthorName = "Aloïs Rautureau",
    feedAuthorEmail = "alois.rautureau@ens-rennes.fr",
    feedRoot = "https://aloisrtr.github.io/thinkingrocks"
}    

pandocCompilerWithStyle :: Style -> Compiler (Item String)
pandocCompilerWithStyle s = 
    pandocCompilerWith
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
            {
                writerHighlightStyle = Just s
            }

createBlog style = 
    hakyllWith config $ do
        create ["rss.xml"] $ do
            route idRoute
            compile $ do
                let feedCtx = postCtx `mappend` bodyField "description"
                posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
                renderRss rssFeedConfiguration feedCtx posts

        match "files/*/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "files/*" $ do
            route   idRoute
            compile copyFileCompiler
            
        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        create ["css/syntax.css"] $ do
            route idRoute
            compile $ do
                makeItem $ styleToCss style

        match "posts/*" $ do
            route $ setExtension "html"
            compile $ pandocCompilerWithStyle style
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        create ["blog.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let blogCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Blog"                `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                    >>= loadAndApplyTemplate "templates/default.html" blogCtx
                    >>= relativizeUrls


        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let indexCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler

main :: IO ()
main = do
    style <- pandocCodeStyle
    createBlog style
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
