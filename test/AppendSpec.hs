{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module AppendSpec where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Text.XML as X
import Data.Maybe (fromMaybe)

import Text.ODT hiding (italicPara)
import ConstantsSpec (
    loadArchive
  , testText
  , boldItalicStyle
  , boldItalicSpan
  , italicParaStyle
  , italicPara
  , odtList)


appendBoldItalicTextStyleWithSpanSad :: IO Bool
appendBoldItalicTextStyleWithSpanSad = 
  hasTextStyle boldItalicStyle . contentDoc <$> loadArchive "example"

appendBoldItalicTextStyleWithSpanHappy :: IO Bool
appendBoldItalicTextStyleWithSpanHappy = do
  archive <- loadArchive "example"
  let orig = contentDoc archive
  let new = appendODT boldItalicSpan orig
  return $ not (hasTextStyle boldItalicStyle orig)
            && hasTextStyle boldItalicStyle new
            && spanCount new == spanCount orig + 1
            && getText orig /= ""
            && getText new == getText orig <> testText

prependBoldItalicTextStyleWithSpanHappy :: IO Bool
prependBoldItalicTextStyleWithSpanHappy = do
  archive <- loadArchive "example"
  let orig = contentDoc archive
  let new = prependODT boldItalicSpan orig
  return $ not (hasTextStyle boldItalicStyle orig)
            && hasTextStyle boldItalicStyle new
            && spanCount new == spanCount orig + 1
            && getText orig /= ""
            && getText new == testText <> getText orig

appendItalicParaStyleWithParaSad :: IO Bool
appendItalicParaStyleWithParaSad = 
  hasParaStyle italicParaStyle . contentDoc <$> loadArchive "example"

appendItalicParaStyleWithParaHappy :: IO Bool
appendItalicParaStyleWithParaHappy = do
  archive <- loadArchive "example"
  let orig = contentDoc archive
  let new = appendODT italicPara orig

  return $ not (hasParaStyle italicParaStyle orig)
            && hasParaStyle italicParaStyle new 
            && paraCount new == paraCount orig + 1
            && getText orig /= ""
            && getText new == getText orig <> testText

prependItalicParaStyleWithParaHappy :: IO Bool
prependItalicParaStyleWithParaHappy = do
  archive <- loadArchive "example"
  let orig = contentDoc archive
  let new = prependODT italicPara orig

  return $ not (hasParaStyle italicParaStyle orig)
            && hasParaStyle italicParaStyle new 
            && paraCount new == paraCount orig + 1
            && getText orig /= ""
            && getText new == testText <> getText orig

-- Test that style that exists on stylesdoc is chosen
-- from the content doc
-- And that the style name of the style in the text node
-- is that of the content doc
appendParaToArchive :: IO Bool
appendParaToArchive = do
  archive <- loadArchive "example"
  let archive' = appendODT italicPara archive
  let cdoc = contentDoc archive'
  let sdoc = stylesDoc archive'

  return $ not (hasParaStyle italicParaStyle sdoc)
            && hasParaStyle italicParaStyle cdoc
            && (head . T.unpack . head . getParaStyleNamesFromParaNodes . getLastPara $ cdoc) == 'P'

-- Test that style that exists on stylesdoc is not chosen
-- from the content doc
-- And that the style name of the style in the text node
-- is that of the styles doc, and not of the content doc
appendParaToArchive' :: IO Bool
appendParaToArchive' = do
  archive <- loadArchive "example"
  let cdoc = contentDoc archive
  let sdoc = stylesDoc archive

  let sdoc' = appendODT (toODT italicParaStyle) sdoc
  let archive' = Archive {contentDoc = cdoc, stylesDoc = sdoc'}

  let archive'' = appendODT italicPara archive'

  let cdoc'' = contentDoc archive''
  let sdoc'' = stylesDoc archive''  

  return $ not (hasParaStyle italicParaStyle cdoc'')
            && hasParaStyle italicParaStyle sdoc''
            && (head . getParaStyleNamesFromParaNodes . getLastPara $ cdoc'') == (fromMaybe "" $ paraStyleName italicParaStyle)

-- Test that if you append a style to the styles doc
-- if it has the same attributes
-- it is only appended once
appendStyleWithSameAttributesToStyleDocOnlyOnce :: IO Bool
appendStyleWithSameAttributesToStyleDocOnlyOnce = do
  archive <- loadArchive "example"
  let sdoc = appendODT (toODT italicParaStyle) . stylesDoc $ archive
  let sdoc' = appendODT (toODT italicParaStyle) sdoc
  return $ (length . getParaStylesWithName "italicPara" $ sdoc') == 1

-- Test that appending directly to an ODT yields the same
-- result as appending to a doc
appendToODTEqAppendToDoc :: IO Bool
appendToODTEqAppendToDoc = do
  archive <- loadArchive "example"
  let doc = contentDoc archive
  let odt = getODT doc

  let docodt = getODT $ appendODT [italicPara, boldItalicSpan, italicPara, boldItalicSpan] doc
  let odtodt = odt <> [italicPara, boldItalicSpan, italicPara, boldItalicSpan]

  return $ docodt == odtodt

-- Test that appending directly to an ODT yields the same
-- result as appending to a doc
prependToODTEqAppendToDoc :: IO Bool
prependToODTEqAppendToDoc = do
  archive <- loadArchive "example"
  let doc = contentDoc archive
  let odt = getODT doc

  let docodt = getODT $ prependODT [italicPara, boldItalicSpan, italicPara, boldItalicSpan] doc
  let odtodt = [italicPara, boldItalicSpan, italicPara, boldItalicSpan] <> odt

  return $ docodt == odtodt

