{-# LANGUAGE OverloadedStrings, UnboxedTuples #-}

module Blog.Util.Markdown
(
  stripMarkdown,
  truncateMarkdown,
  parseMarkdown
)
where

{-
  Intelligently manhandle markdown

  stripMarkdown -> removes all markdown formatting from a markdown document
  truncateMarkdown -> truncates markdown based on length of rendered text
-}

import Cheapskate
import Data.Text (Text)
import qualified Data.Text as T
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import qualified Data.Foldable as F

parseMarkdown :: Text -> Doc
parseMarkdown = markdown def

stripMarkdown :: Doc -> Text
stripMarkdown (Doc _ blocks) = F.fold $ fmap blockToText blocks
  where
    inlinesToText = F.fold . fmap inlineToText
    blockToText (Para inlines) = inlinesToText inlines
    blockToText _ = ""
    inlineToText (Str txt) = txt
    inlineToText (Code txt) = txt
    inlineToText (Emph inlines) = inlinesToText inlines
    inlineToText (Strong inlines) = inlineToText $ Emph inlines
    inlineToText (Link inlines _ _) = inlineToText $ Emph inlines
    inlineToText SoftBreak = inlineToText LineBreak
    inlineToText LineBreak = " "
    inlineToText Space = " "
    inlineToText _ = ""
    
truncateMarkdown :: Int -> Doc -> Doc
truncateMarkdown len (Doc o bs) = Doc o $ loop len (F.toList bs) S.empty
  where
    lenInlines = sum . fmap lenInline
    lenBlocks = sum . fmap lenBlock
    
    lenInline (Str txt) = T.length txt
    lenInline (Code txt) = T.length txt
    lenInline Space = 1
    lenInline LineBreak = 0
    lenInline SoftBreak = 0
    lenInline (Emph inlines) = lenInlines inlines
    lenInline (Strong inlines) = lenInlines inlines
    lenInline (Link inlines _ _) = lenInlines inlines
    lenInline (Entity txt) = T.length txt
    lenInline (Image _ _ _) = 0
    lenInline (RawHtml html) = T.length html

    lenBlock (Para inlines) = lenInlines inlines
    lenBlock (Header _ inlines) = lenInlines inlines
    lenBlock (Blockquote blocks) = lenBlocks blocks
    lenBlock (List _ _ blockList) = sum $ map lenBlocks blockList
    lenBlock (CodeBlock _ code) = T.length code
    lenBlock (HtmlBlock htmlText) = T.length htmlText
    lenBlock HRule = 0

    -- FIXME: doesn't match all cases (missing @loop (I# _) (_:_) _@)
    --        This may be a GHC bug.
    loop :: Int -> [Block] -> Seq Block -> Seq Block
    loop _    []     accum = accum
    loop 0    _      accum = accum
    loop rlen (x:xs) accum
      | lenBlock x < rlen = loop (rlen-(lenBlock x)) xs (accum |> x)
      | lenBlock x > rlen = accum |> x