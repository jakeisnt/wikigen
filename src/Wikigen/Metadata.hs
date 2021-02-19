{-#LANGUAGE DeriveGeneric, OverloadedStrings, DataKinds, TypeOperators, NoImplicitPrelude #-}

module Wikigen.Metadata
       ( getMetadata
       ) where

import Wikigen.Types
import Text.Pandoc
import Universum

-- Extracts globally relevant metadata from pandoc objects
getMetadata :: [Pandoc] -> Metadata
getMetadata docs = mapMetadata $ map (\(Pandoc meta block) ->
                                        mergeMetadata
                                        (getMetaMetadata meta)
                                        (mapMetadata $ map getBlockMetadata block)) docs
  where
    -- TODO: this format is highly dependent on the file format. see what Org needs!
    getMetaMetadata :: Meta -> Metadata
    getMetaMetadata metamap = mtMeta

    -- get metadata from a Block
    getBlockMetadata :: Block -> Metadata
    getBlockMetadata b = case b of
      Plain p -> mapMetadata $ map getInlineMetadata p
      Para p -> mapMetadata $ map getInlineMetadata p
      LineBlock p -> mapMetadata $ map (mapMetadata . map getInlineMetadata) p

      CodeBlock attr text -> mtMeta
      RawBlock fmt txt -> mtMeta
      BlockQuote blocks -> mapMetadata $ map getBlockMetadata blocks
      OrderedList lmeta blocks -> mapMetadata $ map (mapMetadata . (map getBlockMetadata)) blocks
      BulletList blocks -> mapMetadata $ map (mapMetadata . (map getBlockMetadata)) blocks
      DefinitionList inlineBlocks -> mtMeta -- get definitions, enable referencing them elsewhere in wiki? how is that difference from a quoteback?
      Header level attrs txts -> mtMeta
      Table attr cap cols thead tbodies tfoot -> mtMeta
      Div attr blocks -> mapMetadata $ map getBlockMetadata blocks

      HorizontalRule -> mtMeta -- not sure what this is!
      Null -> mtMeta

    -- get metadata from an Inline
    getInlineMetadata :: Inline -> Metadata
    getInlineMetadata inline = case inline of
      Str t -> mtMeta
      Emph inlines -> mapMetadata $ map getInlineMetadata inlines
      Underline inlines -> mapMetadata $ map getInlineMetadata inlines
      Strong inlines -> mapMetadata $ map getInlineMetadata inlines
      Strikeout inlines -> mapMetadata $ map getInlineMetadata inlines
      Superscript inlines -> mapMetadata $ map getInlineMetadata inlines
      Subscript inlines -> mapMetadata $ map getInlineMetadata inlines
      SmallCaps inlines -> mapMetadata $ map getInlineMetadata inlines
      Quoted qtype inlines -> mapMetadata $ map getInlineMetadata inlines
      Cite citations inlines -> mapMetadata $ map getInlineMetadata inlines
      Code attr txt -> mtMeta
      Math mtyp txt -> mtMeta -- tex math literal
      RawInline fmt txt -> mtMeta
      Link attr inlines tgt -> mapMetadata $ map getInlineMetadata inlines -- (alt text, list of inlines in text label, link target)
      Image attr inlines target -> mapMetadata $ map getInlineMetadata inlines -- same as link
      Note blocks -> mapMetadata $ map getBlockMetadata blocks
      Span attr inlines -> mapMetadata $ map getInlineMetadata inlines -- generic inline container; unused by org?

      Space -> mtMeta
      SoftBreak -> mtMeta
      LineBreak -> mtMeta

    -- get metadata from an attribute
    getAttrMetadata :: Attr -> Metadata
    getAttrMetadata (ident, classes, kvs) = mtMeta
