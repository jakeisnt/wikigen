{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Wikigen.Parser
  ( ParsingException (..),
    parseOrg,
    runParser,
  )
where

import Wikigen.Ast
import qualified Data.Attoparsec.Text as A
import Data.Char (isSpace)
import qualified Data.OrgMode.Parse as O
import qualified Data.OrgMode.Types as O
import qualified Data.Text as T
import Data.Time
  ( LocalTime (..),
    getZonedTime,
    zonedTimeToLocalTime,
  )
import Data.Time.Calendar ()
import Universum

-- https://github.com/volhovm/orgstat/blob/master/src/OrgStat/Parser.hs
----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

newtype ParsingException = ParsingException Text
  deriving (Show, Typeable)

instance Exception ParsingException

----------------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------------

parseOrgSection :: Text -> O.Section
parseOrgSection initialText =
  case A.parseOnly O.parseSection initialText of
    -- if parsing fails, fall back to storing as plain text
    Left msg -> error $ show msg -- O.Section [O.Paragraph [O.Plain $ T.pack message]]
    Right sec -> sec

parseOrg :: LocalTime -> [Text] -> A.Parser Org
parseOrg curTime todoKeywords =
  convertDocument <$> O.parseDocumentWithKeywords todoKeywords
  where
    convertDocument :: O.Document -> Org
    convertDocument (O.Document textBefore headings) =
      let fileLvlTags = extractFileTags textBefore
          addTags t = ordNub $ fileLvlTags <> t
          -- TODO the header information is weirdly parsed here
          (title, initialText) = fromMaybe ("", textBefore) $ extractTitle textBefore
          section = convertSection $ parseOrgSection initialText
          o =
            Org
              { _orgTitle = title,
                _orgStructuredText = section,
                _orgTags = [],
                _orgClocks = [],
                _orgSubtrees = map convertHeading headings
              }
       in o & traverseTree . orgTags %~ addTags

    convertHeading :: O.Headline -> Org
    convertHeading headline =
      let section = O.section headline
       in Org
            { _orgTitle = O.title headline,
              _orgStructuredText = convertSection section,
              _orgTags = O.tags headline,
              _orgClocks = [], -- getClocks $ O.section headline,
              _orgSubtrees = map convertHeading $ O.subHeadlines headline
            }

    convertSection :: O.Section -> OrgSection
    convertSection O.Section {O.sectionContents = section} = OrgSection $ map getOrgContent section
      where
        getOrgContent :: O.Content -> OrgContent
        getOrgContent content = case content of
          O.OrderedList items -> OrgOrderedList $ map (\(O.Item contents) -> OrgItem $ map getOrgContent contents) items
          O.UnorderedList items -> OrgUnorderedList $ map (\(O.Item contents) -> OrgItem $ map getOrgContent contents) items
          O.Paragraph markup -> OrgParagraph $ map getOrgMarkup markup

        getOrgMarkup :: O.MarkupText -> Markup
        getOrgMarkup markup = case markup of
          O.Plain t -> OrgPlain t
          O.LaTeX t -> OrgLaTeX t
          O.Verbatim t -> OrgVerbatim t
          O.Code t -> OrgCode (Language "TODO") (Output False) t
          O.Bold m -> OrgBold $ map getOrgMarkup m
          O.Italic m -> OrgItalic $ map getOrgMarkup m
          O.UnderLine m -> OrgUnderLine $ map getOrgMarkup m
          O.Strikethrough m -> OrgStrikethrough $ map getOrgMarkup m
          O.HyperLink {O.link = l, O.description = d} ->
            case takeWhile (/= ']') $ T.unpack l of
              "file:" ->
                -- 5: length of 'file:'
                let path = T.unpack $ T.drop 5 l
                 in OrgFileLink {filepath = path, description = d}
              _ -> OrgHyperLink {link = l, description = d}
    -- TODO: catch insecure links here!

    extractFileTags (T.lines -> inputLines) =
      let prfx = "#+FILETAGS: "
          matching =
            map (T.drop (length prfx)) $ filter (T.isPrefixOf prfx) inputLines
          toTags (T.strip -> line) =
            let parts = filter (not . T.null) $ T.splitOn ":" line
                -- Correct tag shouldn't contain spaces inside
                correctPart p = not $ T.any isSpace p
             in if all correctPart parts then parts else []
       in ordNub $ concatMap toTags matching

    extractTitle (T.lines -> inputLines) =
      let prfx = "#+TITLE: "
          matching = map (T.drop (length prfx)) $ filter (T.isPrefixOf prfx) inputLines
          leftover = T.concat $ filter (not . T.isPrefixOf prfx) inputLines
          result = nonEmpty matching
       in case result of
            Just title -> Just (head title, leftover)
            Nothing -> Nothing

-- Throw parsing exception if it can't be parsed (use Control.Monad.Catch#throwM)
runParser :: (MonadIO m, MonadThrow m) => [Text] -> Text -> m Org
runParser todoKeywords t = do
  localTime <- liftIO $ zonedTimeToLocalTime <$> getZonedTime
  case A.parseOnly (parseOrg localTime todoKeywords) t of
    Left err -> throwM $ ParsingException $ T.pack err
    Right res -> pure res
