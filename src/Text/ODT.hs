module Text.ODT (
      Archive(..)
    , appendToODTWithExtraction
    , defaultODTFileOptions
    , Doc(..)
    , extractAndLoadArchiveFromZip
    , FontStyle(..)
    , getFirstODT
    , getFirstPara
    , getLastPara
    , getParas
    , getParaStyleNamesFromParaNodes
    , getParaStylesWithName
    , getSpans
    , getText
    , getTextStyleNamesFromParaNodes
    , HasContentODT(..)
    , HasODT(..)
    , HasParaStyles(..)
    , HasTextStyles(..)
    , IsODT(..)
    , IsStyle(..)
    , loadArchiveFromZip
    , ODT(..)
    , ODTFileOptions(..)
    , paraCount
    , ParaStyle(..)
    , replaceContentDocODT
    , writeNewODT
    , writeNewODTWithStyles
    , spanCount
    , TextStyle(..)
    , TextProps(..)
    , updateODTFile
    
    -- New styles
    , newParaStyle
    , newTextProps
    , newTextStyle

    -- Text styles
    , bold
    , italic
    , underline
    , normal

    -- Paragraph styles
    , normalPara
    , italicPara
    , italicwritePara
    , italicParaODTM

    -- Text units
    , para
    , str
    , textspan
    , writeNewPara
    , writePara
    , writeTextSpan
    ) where

import Text.ODT.Extract
import Text.ODT.Compress
import Text.ODT.File
import Text.ODT.XML.Prettify
import Text.ODT.Utils.Types (
      IsText(..)
    , Stringable(..))
import Text.ODT.ODT
import Text.ODT.Doc
import Text.ODT.Archive
import Text.ODT.Query
import Text.ODT.TextUnits
import qualified Text.ODT.ODT as ODTType
import Text.ODT.Style
import Text.ODT.Style.TextStyles
import Text.ODT.Style.ParaStyles



