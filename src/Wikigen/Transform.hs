{-#LANGUAGE DeriveGeneric, OverloadedStrings, DataKinds, TypeOperators, NoImplicitPrelude #-}

module Wikigen.Transform
       ( transformAst ) where

import Text.Pandoc
import Wikigen.Types
import Universum
import System.FilePath
import qualified Data.Text as T

-- Transform the Pandoc document AST
transformAst :: Metadata -> Pandoc -> Pandoc
transformAst meta (Pandoc m block) = Pandoc m $ map (transformBlock meta) block
  where
    transformBlock :: Metadata -> Block -> Block
    transformBlock meta b = case b of
      Plain p -> Plain $ map (transformInline meta) p
      Para p -> Para $ map (transformInline meta) p
      LineBlock p -> LineBlock $ map (map (transformInline meta)) p

      BlockQuote blocks ->  BlockQuote $ map (transformBlock meta) blocks
      OrderedList lmeta blocks -> OrderedList lmeta $ map (map (transformBlock meta)) blocks
      BulletList blocks ->  BulletList $ map (map (transformBlock meta)) blocks
      DefinitionList inlineBlocks -> b -- get definitions, enable referencing them elsewhere in wiki? how is that difference from a quoteback?
      Header level attrs txts -> b
      CodeBlock attr text -> b
      RawBlock fmt txt -> b
      Table attr cap cols thead tbodies tfoot -> b
      Div attr blocks ->  Div attr $ map (transformBlock meta) blocks

      HorizontalRule -> b -- not sure what this is!
      Null -> b

    transformInline :: Metadata -> Inline -> Inline
    transformInline meta inline = case inline of
      Str t -> inline
      Emph inlines ->  Emph $ map (transformInline meta) inlines
      Underline inlines -> Underline $ map (transformInline meta) inlines
      Strong inlines -> Strong $ map (transformInline meta) inlines
      Strikeout inlines -> Strikeout $ map (transformInline meta) inlines
      Superscript inlines -> Superscript $ map (transformInline meta) inlines
      Subscript inlines -> Subscript $ map (transformInline meta) inlines
      SmallCaps inlines -> SmallCaps $ map (transformInline meta) inlines
      Quoted qtype inlines -> Quoted qtype $ map (transformInline meta) inlines
      Cite citations inlines -> Cite citations $ map (transformInline meta) inlines
      Code attr txt -> inline
      Math mtyp txt -> inline -- tex math literal
      RawInline fmt txt -> inline
      -- (alt text, list of inlines in text label, link target)
      Link attr inlines tgt ->  Link attr (map (transformInline meta) inlines) (transformLinkTarget meta tgt) 
      Image attr inlines target ->  Image attr (map (transformInline meta) inlines) target -- same as link
      Note blocks -> Note $ map (transformBlock meta) blocks
      Span attr inlines -> Span attr $ map (transformInline meta) inlines -- generic inline container; unused by org?
      Space -> inline
      SoftBreak -> inline
      LineBreak -> inline

    transformLinkTarget :: Metadata -> Target -> Target
    transformLinkTarget m (url, title) = (T.pack $
                                         let uri = T.unpack url in
                                          case takeExtension uri of
                                            ".org" -> uri -<.> ".html"
                                            _ -> uri
                                         , title)


