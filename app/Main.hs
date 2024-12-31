{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Text.XML as XML

import Text.ODT.Diagnostics.Utils ( unzipOdt, prettifyOdt )

import Text.ODT.File ( concatPath )
import Text.ODT.XML.Prettify ( prettifyFile )
import qualified Text.ODT.Zip.Zip as Zip 
import Text.ODT.ODT ( IsList(toList), HasODT(getODT), ODT )
import Text.ODT.Doc ( Doc(odt), IsXMLDoc(fromXMLDoc, toXMLDoc) )
import Text.ODT.Query ( getLastPara, getText, paraCount )
import qualified Text.ODT.Ops as ODT
import Text.ODT.Style
    ( ParaStyle(paraStyleName, paraTextProps),
      newTextProps,
      TextProps(fontStyle),
      FontStyle(Italic),
      newParaStyle )

import qualified Text.ODT.Style.TextStyles as TextStyles

exampleFilename = "example2"
folderpath = "../doctools-data" 
rootPath = concatPath folderpath []
examplePath = concatPath folderpath [exampleFilename]

main :: IO ()
main = do
    unzipOdt folderpath exampleFilename folderpath
    
    contentXmlDoc <- XML.readFile XML.def (examplePath "content" "xml")
    stylesXmlDoc <- XML.readFile XML.def (examplePath "styles" "xml")
    
    let contentOdtDoc = fromXMLDoc contentXmlDoc

    let contentOdt = getODT contentOdtDoc <> ODT.span TextStyles.underline " Some underlined text."

    let contentXmlDoc' = toXMLDoc $ contentOdtDoc {odt = contentOdt}

    XML.writeFile XML.def (examplePath "content" "xml") contentXmlDoc'

    Zip.zipODT (rootPath exampleFilename "odt") [examplePath "content" "xml", examplePath "styles" "xml"] (rootPath "modified" "odt")
