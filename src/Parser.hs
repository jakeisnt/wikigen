{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parser
  ( ParsingException (..),
    parseOrg,
    runParser,
  )
where

import Ast
import qualified Data.Attoparsec.Text as A
import Data.Char (isSpace)
import qualified Data.OrgMode.Parse as O
import qualified Data.OrgMode.Types as O
import qualified Data.Text as T
import Data.Time
  ( LocalTime (..),
    TimeOfDay (..),
    fromGregorian,
    getZonedTime,
    zonedTimeToLocalTime,
  )
import Data.Time.Calendar ()
import System.FilePath
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
    Left message -> error "bad parse" -- O.Section [O.Paragraph [O.Plain $ T.pack message]]
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
              -- _orgText = O.sectionParagraph $ O.section headline,
              _orgStructuredText = convertSection section,
              _orgTags = O.tags headline,
              -- _orgClocks = getClocks $ O.section headline,
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
          O.Bold markup -> OrgBold $ map getOrgMarkup markup
          O.Italic markup -> OrgItalic $ map getOrgMarkup markup
          O.UnderLine markup -> OrgUnderLine $ map getOrgMarkup markup
          O.Strikethrough markup -> OrgStrikethrough $ map getOrgMarkup markup
          O.HyperLink {O.link = l, O.description = d} ->
            case takeWhile (/= ']') $ T.unpack l of
              "file:" ->
                -- 5: length of 'file:'
                let path = T.unpack $ T.drop 5 l
                 in OrgFileLink {Ast.filepath = path, Ast.description = d}
              _ -> OrgHyperLink {Ast.link = l, Ast.description = d}
    -- TODO: catch insecure links here!

    mapEither :: (a -> Either e b) -> ([a] -> [b])
    mapEither f xs = rights $ map f xs

    -- getClocks :: O.Section -> [Clock]
    -- getClocks section =
    --   mapMaybe convertClock $
    --     concat
    --       [ O.sectionClocks section,
    --         O.unLogbook (O.sectionLogbook section),
    --         mapEither
    --           (A.parseOnly O.parseClock)
    --           ( concatMap (lines . O.contents) (O.sectionContents section)
    --               ++ lines (O.sectionContents section)
    --           )
    --       ]

    -- convert clocks from orgmode-parse format, returns Nothing for clocks
    -- without end time or time-of-day
    convertClock :: O.Clock -> Maybe Clock
    convertClock (O.Clock (Just (O.Timestamp start _active (Just end)), _duration)) =
      Clock <$> convertDateTime start <*> convertDateTime end
    convertClock (O.Clock (Just (O.Timestamp start _active Nothing), _duration)) =
      Clock <$> convertDateTime start <*> pure curTime
    convertClock _ = Nothing

    -- Nothing for DateTime without time-of-day
    convertDateTime :: O.DateTime -> Maybe LocalTime
    convertDateTime O.DateTime {O.yearMonthDay = O.YearMonthDay year month day, O.hourMinute = Just (hour, minute)} =
      Just $
        LocalTime
          (fromGregorian (toInteger year) month day)
          (TimeOfDay hour minute 0)
    convertDateTime _ = Nothing

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
          toTags (T.strip -> line) = filter (not . T.null) $ T.splitOn ":" line
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
