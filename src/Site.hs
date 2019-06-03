{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude ()
import Prelude.Compat
import Control.Lens hiding (Context)

import Control.Monad (when)
import Control.Monad.State.Strict (State, runState)
import Data.List (isPrefixOf)
import Text.Pandoc (Pandoc, Block (..))
import Text.Pandoc.Walk (walk)
import Skylighting (Syntax, parseSyntaxDefinition)

import Hakyll

import qualified Data.Map as Map
import qualified Text.HTML.TagSoup as TS
import qualified Text.HTML.TagSoup.Tree as TS
import qualified Text.Pandoc.Options as PO
import qualified Text.Pandoc.Highlighting as PH

import qualified Hakyll.Contrib.LaTeX as LaTeX
import qualified Image.LaTeX.Render as LaTeX
import qualified Image.LaTeX.Render.Pandoc as LaTeX

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Oleg's gists"
    , feedDescription = "Haskell related gists"
    , feedAuthorName  = "Oleg Grenrus"
    , feedAuthorEmail = "oleg.grenrus@iki.fi"
    , feedRoot        = "http://oleg.fi/gists"
    }

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = LaTeX.initFormulaCompilerDataURI 1000 LaTeX.defaultEnv >>= main'

main' :: (LaTeX.PandocFormulaOptions -> Pandoc -> Compiler Pandoc) -> IO ()
main' renderFormulae = do
  cabalSyntax <- parseSyntaxDefinition "syntax/cabal.xml" >>= either fail pure
  hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- match "js/*" $ do
    --     route   idRoute
    --    compile copyFileCompiler

    -- build up tags
    tags <- buildTags postsPattern (fromCapture "tags/*.html")

    tagsRules tags $ \tag patt -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll patt
            let ctx = mconcat
                  [ constField "title" title
                  , constField "subtitle" ""
                  , listField "posts" postCtx (return posts)
                  , defaultContext
                  ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match postsPattern $ do
        route $ setExtension "html"
        let compiler
                = pandocCompilerWithTransformM readerOpts (writerOpts cabalSyntax)
                $ renderFormulae pandocFormulaOptions
                . pandocTransform
        compile $ compiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= saveSnapshot "content"
            >>= withItemBody (pure . withTagList makeToc)
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll postsPattern
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    constField "subtitle" ""                 `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll postsPattern
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Oleg Grenrus"        `mappend`
                    constField "subtitle" "<p>programmer</p>"   `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderAtom feedConfiguration feedCtx posts

postsPattern :: Pattern
postsPattern = fromRegex "^posts/.*\\.(md|lhs|tex)$"

-------------------------------------------------------------------------------
-- Latex
-------------------------------------------------------------------------------

pandocFormulaOptions :: LaTeX.PandocFormulaOptions
pandocFormulaOptions = def
    { LaTeX.formulaOptions = fopts
    }
  where
    def = LaTeX.defaultPandocFormulaOptions
    fopts mt = (LaTeX.formulaOptions def mt)
        { LaTeX.preamble = concat
            [ "\\usepackage{amsmath}"
            , "\\usepackage{amssymb}"
            , "\\usepackage{amsthm}"
            , "\\usepackage{amsfonts}"
            , "\\usepackage{stmaryrd}"
            , "\\usepackage{mathpazo}"
            , "\\usepackage{mathtools}"
            -- proofs (find-right-laws)
            , "\\newcommand{\\equivvia}[1]{\\equiv\\!\\!\\langle\\ #1\\ \\rangle}"
            ]
        }

-------------------------------------------------------------------------------
-- Pandoc
-------------------------------------------------------------------------------

writerOpts :: Syntax ->  PO.WriterOptions
writerOpts cabalSyntax = PO.def
    { PO.writerHighlightStyle = Just PH.pygments -- kate
    , PO.writerSyntaxMap      = Map.insert "cabal" cabalSyntax $ PO.writerSyntaxMap PO.def
    -- , PO.writerHtml5 = True
    }

readerOpts :: PO.ReaderOptions
readerOpts = PO.def
    { PO.readerExtensions = PO.githubMarkdownExtensions
        & PO.disableExtension PO.Ext_hard_line_breaks
        & PO.enableExtension PO.Ext_fenced_code_attributes
        & PO.enableExtension PO.Ext_tex_math_dollars
        & PO.enableExtension PO.Ext_literate_haskell
        & PO.enableExtension PO.Ext_footnotes
    }

pandocTransform :: Pandoc -> Pandoc
pandocTransform = walk (map block) where
    block :: Block -> Block
    block (CodeBlock (i, cs, kv) x) | null cs  = CodeBlock (i, ["haskell"], kv) x
    block b = b

-------------------------------------------------------------------------------
-- Posts
-------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    constField "subtitle" ""    `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

-------------------------------------------------------------------------------
-- TOC and other post-processing
-------------------------------------------------------------------------------

makeToc :: [TS.Tag String] -> [TS.Tag String]
makeToc tags = TS.flattenTree tt4
  where
    tt0 = TS.tagTree tags
    (tt1, S _ ll hs) = runState
        (traverseOf (traverse . deepOf platedTagTree h2or3) processHeader tt0)
        initS

    tt2 = tt1 & traverse . deepOf platedTagTree tocDiv %~ createToc
    tt3 = tt2 & traverse . deepOf (notA . platedTagTree) code %~ createCodeLink

    -- not a toc, but changing a -> span in <pre><code>
    tt4 = tt3 & traverse . deepOf platedTagTree preTag %~ changeAtoSpan

    -- process headers
    --
    -- * add identifer if not exists
    --
    -- * collect links
    --
    -- * add anchors
    --
    processHeader :: TS.TagTree String -> State S (TS.TagTree String)
    processHeader (TS.TagBranch s attrs ts) = do
        let innerText = textFrom ts

        -- identifier
        (attrs', idAttr) <- case lookup "id" attrs of
            Just a  -> pure (attrs, a)
            Nothing -> do
                n <- sectionCounter <%= (+1)
                let a = "s:" ++ show n
                pure (("id", a) : attrs, a)

        -- link lookup
        when ("v:" `isPrefixOf` idAttr || "t:" `isPrefixOf` idAttr) $
            linkLookup . at (drop 2 idAttr) ?= idAttr

        -- h2 which looks like "class"
        let identChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
        when (s == "h2" && all (`elem` identChars) innerText) $
            linkLookup . at innerText ?= idAttr

        -- add into sections
        sections %= ((s, idAttr, innerText) :)

        -- add anchor
        let anchor = TS.TagBranch "a"
                [("class", "anchor"), ("href", "#" ++ idAttr)]
                [TS.TagLeaf $ TS.TagText "#"]
        let ts' = anchor : ts

        pure (TS.TagBranch s attrs' ts')
    processHeader tt                    = pure tt

    textFrom :: [TS.TagTree String] -> String
    textFrom = view (traverse . deepOf platedTagTree _TextTagLeaf)

    createToc :: TS.TagTree String -> TS.TagTree String
    createToc (TS.TagBranch "div" attrs _) =
        TS.TagBranch "div" attrs $ TS.tagTree $
            let (s, ts) = go "h2" (reverse hs)
            in TS.TagOpen "ul" [] : ts
                ++ [ TS.TagClose "ul" | s == "h3" ]
                ++ [ TS.TagClose "ul" ]
      where
        go :: String -> [(String, String, String)] -> (String, [TS.Tag String])
        go s [] = (s, [])
        go s ((s', idAttr, t) : ts) = go s' ts & _2 %~ \ts' ->
            [ TS.TagClose "ul" | s' == "h2", s == "h3" ] ++
            [ TS.TagOpen "ul" [] | s' == "h3", s == "h2" ] ++
            [ TS.TagOpen "li" []
            , TS.TagOpen "a" [("href", "#" ++ idAttr)]
            , TS.TagText t
            , TS.TagClose "a"
            , TS.TagClose "li"

            ] ++
            ts'
    createToc t = t

    createCodeLink :: TS.TagTree String -> TS.TagTree String
    createCodeLink (TS.TagBranch "code" [] ts@[TS.TagLeaf (TS.TagText c)])
        | Just idAttr <- ll ^. at c = TS.TagBranch "code" []
            [ TS.TagBranch "a" [("href", "#" ++ idAttr)] ts ]

    createCodeLink t = t

    changeAtoSpan :: TS.TagTree String -> TS.TagTree String
    changeAtoSpan tt = tt & tagTreeStrs %~ change where
        change "a" = "span"
        change str = str

    h2or3 :: Traversal' (TS.TagTree String) (TS.TagTree String)
    h2or3 = tag (`elem` ["h2", "h3"]) (const True)

    tocDiv :: Traversal' (TS.TagTree String) (TS.TagTree String)
    tocDiv = tag (== "div") (== [("id","toc")])

    notA :: Traversal' (TS.TagTree String) (TS.TagTree String)
    notA = tag (/= "a") (const True)

    code :: Traversal' (TS.TagTree String) (TS.TagTree String)
    code = tag (== "code") (const True)

    preTag :: Traversal' (TS.TagTree String) (TS.TagTree String)
    preTag = tag (== "pre") (const True)

    tag :: Applicative f
        => (String -> Bool)              -- tag name
        -> ([(String, String)] -> Bool)  -- attributes
        -> LensLike' f (TS.TagTree String) (TS.TagTree String)
    tag ps pa f t@(TS.TagBranch s a _) | ps s, pa a = f t
    tag _  _  _ t = pure t

data S = S
    { _sectionCounter :: Int
    , _linkLookup     :: Map.Map String String
    , _sections       :: [(String, String, String)] -- tag, id, content
    }

sectionCounter :: Lens' S Int
sectionCounter = lens _sectionCounter $ \s x -> s { _sectionCounter = x }

linkLookup :: Lens' S (Map.Map String String)
linkLookup = lens _linkLookup $ \s x -> s { _linkLookup = x }

sections :: Lens' S [(String, String, String)]
sections = lens _sections $ \s x -> s { _sections = x }

initS :: S
initS = S 0 mempty mempty

platedTagTree :: Traversal' (TS.TagTree str) (TS.TagTree str)
platedTagTree f (TS.TagBranch s a ts) = TS.TagBranch s a <$> traverse f ts
platedTagTree _ t@(TS.TagLeaf _)      = pure t -- don't treat leafs

_TextTagLeaf :: Traversal' (TS.TagTree str) str
_TextTagLeaf f (TS.TagLeaf (TS.TagText s)) = TS.TagLeaf . TS.TagText <$> f s
_TextTagLeaf _ t = pure t

-- Branch names
tagTreeStrs :: Traversal' (TS.TagTree str) str
tagTreeStrs f = go where
    go (TS.TagLeaf tag)      = pure (TS.TagLeaf tag)
    go (TS.TagBranch s a ts) = TS.TagBranch <$> f s <*> pure a <*> traverse go ts
