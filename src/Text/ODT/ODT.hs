{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}

module Text.ODT.ODT (
      getLastODT
    , removeLastODT
    , getStylesODT
    , getParaStylesODT
    , getTextStylesODT
    , nextParaStyleName
    , nextTextStyleName
    , paraStyleNameInts
    , styleToODT
    , textStyleNameInts
    , HasContentODT(..)
    , HasODT(..)
    , HasStylesODT(..)
    , HasTextStyles(..)
    , IsList(..)
    , IsNodes(..)
    , IsODT(..)
    , ODT(..)
    , OfficeNodeType(..)
    , TextNodeType(..)
    , TextLeafType(..)
) where

import Text.XML
    ( Document(..)
    , Element(..)
    , Node(..)
    , Name(..)
    , Prologue(..)
    , Miscellaneous(..) )

import GHC.Exts (IsList(..))
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.List as L

import Text.ODT.Utils.Types (
      IsText(..)
    , Stringable(..))

import Text.ODT.XML.Doc ( 
      Namespaces
    , Epilogue )

import Text.ODT.Style
import Text.ODT.ODTXML.Attr
import Text.ODT.ODTXML.Namespace
import Text.ODT.ODTXML.ODTXML
import Text.ODT.ODTXML.Name
import Text.ODT.XML.Types 

import Text.Read

type Id = T.Text
type Parent = Node
type AlwaysInclude = Bool

-- Types

data OfficeNodeType = 
      DocContent 
    | AutoStyles
    | DocStyles
    | FontFaceDecls
    | MasterStyles
    | Scripts
    | Styles
    | Style
    | Body
    | OfficeTextNode
    deriving (Eq, Show)

data StyleNodeType =
      DefStyle
    | FontFace
    | GraphicProps
    | ParaProps
    -- AlwaysInclude is there for when a document is converted to a list, 
    -- to make sure that the style goes back in when concatenated
    | StyleType AlwaysInclude   
    | TabStops
    | TableProps
    | TableRowProps
    | TextPropsNode
    deriving (Eq, Show)

data TextNodeType = 
      P (Maybe ParaStyle)
    | Span (Maybe TextStyle)
    | Note 
    | NoteBody 
    | SequenceDecls
    | SequenceDecl
    deriving (Eq, Show)

data TextLeafType =
      Str
    | NoteCit
    deriving (Eq, Show)

-- An ODT element (in principle) contains:
--      a Type (for pattern matching)
--      an ODTXML node (containing the information for constructing the XML node)
--      an ODT element (comprising the child nodes), i.e.:
--          - either a single node
--          - or an ODTSeq of multiple child nodes
data ODT where
    OfficeNode    :: OfficeNodeType -> ODTXML -> ODT -> ODT
    TextNode      :: TextNodeType -> ODTXML -> ODT -> ODT
    TextLeaf      :: TextLeafType -> ODTXML -> ODT
    StyleNode     :: StyleNodeType -> ODTXML -> ODT -> ODT
    ODTSeq        :: ODT -> ODT -> ODT 
    MiscODT       :: ODTXML -> ODT
    EmptyODT      :: ODT
    deriving Eq

instance Show ODT where
    show (TextLeaf    NoteCit   n     ) = show NoteCit
    show (TextLeaf    typ       n     ) = show typ <> " \"" <> T.unpack (getXMLText n) <> "\"" 
    show (TextNode    typ       _ odt ) = show typ <> " (" <> show odt <> ")"
    show (StyleNode  TextPropsNode n odt )  = show TextPropsNode <> " (" <> show odt <> ")"
    show (StyleNode  typ n odt)  = show typ <> "(" <> (show . getAttrs $ n) <> "\n\t\t" <> show odt <> " "  <> ")"
    show (OfficeNode  typ       n odt ) = show typ <> " (" <> (show . getAttrs $ n) <> show odt <> ")"
    show (MiscODT               n     ) = "MiscODT:" <> (T.unpack $ getLocalName n) 
    show (ODTSeq      odt1    odt2  )   = "ODTSeq " <> "(" <> show odt1 <> ",\n\t" <> show odt2 <> ")"
    show EmptyODT = "_"

-- Monoid instances
instance Semigroup ODT where
    (<>) :: ODT -> ODT -> ODT

    -- APPEND STYLES TO DOCUMENT
    -- If AlwaysInclude is set to True
    -- AlwaysInclude is an override for use when the document is serialized as a list
    -- to make sure that the style goes back in when it is returned to an ODT
    -- content.xml
    OfficeNode AutoStyles n1 odt1 <> StyleNode (StyleType True) n2 odt2 =
        OfficeNode AutoStyles n1 (odt1 <> styleODT)
        where styleODT = StyleNode (StyleType False) n2 odt2

    -- If AlwaysInclude set to False
    -- Find out if the style is already listed
    -- Only add if a new style
    OfficeNode AutoStyles n1 odt1 <> StyleNode (StyleType False) n2 odt2        
        | Nothing <- style = autostyles
        | Just (Left textstyle) <- style = case hasTextStyle textstyle autostyles of
            True -> autostyles
            False -> OfficeNode AutoStyles n1 (odt1 <> styleODT)
        | Just (Right parastyle) <- style = case hasParaStyle parastyle autostyles of
            True -> autostyles
            False -> OfficeNode AutoStyles n1 (odt1 <> styleODT)
            
        where autostyles = OfficeNode AutoStyles n1 odt1
              styleODT = StyleNode (StyleType False) n2 odt2
              style = toStyle styleODT   

    -- styles.xml
    OfficeNode DocStyles n1 odt1 <> StyleNode (StyleType alwaysInclude) n2 odt2 = 
        OfficeNode DocStyles n1 $ odt1 <> StyleNode (StyleType alwaysInclude) n2 odt2
    
    -- Append style to styles doc; only add if style does not already exist
    -- in the styles doc
    OfficeNode Styles n1 odt1 <> StyleNode (StyleType alwaysInclude) n2 odt2
        | Nothing <- style = styles
        | Just (Left textstyle) <- style = case hasTextStyle textstyle styles of
            True -> styles
            False -> OfficeNode Styles n1 (ODTSeq styleODT odt1)
        | Just (Right parastyle) <- style = case hasParaStyle parastyle styles of
            True -> styles
            False -> OfficeNode Styles n1 (ODTSeq styleODT odt1)
        where styles = OfficeNode Styles n1 odt1
              styleODT = StyleNode (StyleType False) n2 odt2
              style = toStyle styleODT

    OfficeNode officeType n1 odt1 <> StyleNode (StyleType alwaysInclude) n2 odt2 = 
        OfficeNode officeType n1 (odt1 <> newStyleODT) 
        
        where   styleODT = StyleNode (StyleType alwaysInclude) n2 odt2
                nextStyleName = nextAutoStyleName styleODT odt1
                n2' = setAttrVal styleNameName nextStyleName n2
                newStyleODT = StyleNode (StyleType alwaysInclude) n2' odt2

    ODTSeq (OfficeNode Styles n1 odt1) (odt2) <> StyleNode (StyleType alwaysInclude) n3 odt3 = 
        ODTSeq (OfficeNode Styles n1 odt1 <> StyleNode (StyleType alwaysInclude) n3 odt3) (odt2)

    ODTSeq (OfficeNode AutoStyles n1 odt1) (odt2) <> StyleNode (StyleType alwaysInclude) n3 odt3 = 
        ODTSeq (OfficeNode AutoStyles n1 odt1 <> StyleNode (StyleType alwaysInclude) n3 odt3) (odt2)

    StyleNode (StyleType alwaysInclude1) n1 odt1 <> StyleNode (StyleType alwaysInclude2) n2 odt2 = 
        ODTSeq (StyleNode (StyleType alwaysInclude1) n1 odt1) (StyleNode (StyleType alwaysInclude2) n2 odt2)
    StyleNode TextPropsNode n1 odt1 <> StyleNode (StyleType alwaysInclude) n2 odt2 = 
        ODTSeq (StyleNode TextPropsNode n1 odt1) (StyleNode (StyleType alwaysInclude) n2 odt2)
    StyleNode (StyleType alwaysInclude) n1 odt1 <> StyleNode TextPropsNode n2 odt2 = 
        StyleNode (StyleType alwaysInclude) n1 (odt1 <> StyleNode TextPropsNode n2 odt2)
    StyleNode ParaProps n1 odt1 <> StyleNode TextPropsNode n2 odt2 = 
        ODTSeq (StyleNode ParaProps n1 odt1) (StyleNode TextPropsNode n2 odt2)
    MiscODT n1 <> StyleNode typ n2 odt2 = ODTSeq (MiscODT n1) (StyleNode typ n2 odt2)

    -- FONT FACE DECLS
    OfficeNode FontFaceDecls n1 odt1 <> StyleNode FontFace n2 odt2 = 
        OfficeNode FontFaceDecls n1 (odt1 <> StyleNode FontFace n2 odt2)
    StyleNode FontFace n1 odt1 <> StyleNode FontFace n2 odt2 = 
        ODTSeq (StyleNode FontFace n1 odt1) (StyleNode FontFace n2 odt2)

    -- SEQUENCE DECLS
    TextNode SequenceDecls n1 odt1 <> TextNode SequenceDecl n2 odt2 = 
        TextNode SequenceDecls n1 (odt1 <> TextNode SequenceDecl n2 odt2)
    TextNode SequenceDecl n1 odt1 <> TextNode SequenceDecl n2 odt2 = 
        ODTSeq (TextNode SequenceDecl n1 odt1) (TextNode SequenceDecl n2 odt2) 

    -- APPENDING / PREPENDING TEXT
    -- Feed elements of ODTSeq to DocContent individually
    OfficeNode DocContent n1 odt1 <> OfficeNode typ n2 odt2 = 
        OfficeNode DocContent n1 (odt1 <> OfficeNode typ n2 odt2)
    OfficeNode  DocContent  n1 odt1 <> ODTSeq odt2 odt3 = 
        (OfficeNode DocContent n1 odt1 <> odt2) <> odt3 
    (ODTSeq odt1 odt2) <> OfficeNode DocContent n3 odt3 = 
        odt1 <> removeLastODT odt2 <> getLastODT odt2 <> OfficeNode DocContent n3 odt3

    -- Style carried with the P node is added to the document before adding the P
    OfficeNode DocContent n1 odt1 <> TextNode (P (Just parastyle)) n2 odt2     = 
        docContent' <> TextNode (P Nothing) n2' odt2
        where   parastyleodt = toODT parastyle
                docContent' = OfficeNode DocContent  n1 odt1 <> parastyleodt
                parastylename = getParaStyleName parastyle docContent'
                n2' = setAttrVal textStyleNameName parastylename n2 

    -- Style carried with the Span is added to the document before adding the TextNode
    OfficeNode DocContent n1 odt1 <> TextNode (Span (Just textStyle)) n2 odt2     = 
        docContent' <> TextNode (Span Nothing) n2' odt2
        where   textStyleOdt = toODT textStyle
                docContent' = OfficeNode DocContent n1 odt1 <> textStyleOdt
                textStyleName = getTextStyleName textStyle docContent'
                n2' = setAttrVal textStyleNameName textStyleName n2 

    -- -- Append the empty style node
    -- OfficeNode DocContent n1 odt1 <> TextNode (Span Nothing) n2 odt2     = 
    --     OfficeNode DocContent n1 (odt1 <> TextNode (Span Nothing) n2 odt2)

    -- TODO: switch n1 and n2
    TextNode (P (Just parastyle)) n2 odt2  <> OfficeNode DocContent n1 odt1   = 
        (TextNode (P Nothing) n2' odt2) <> docContent'
        where   parastyleodt = toODT parastyle
                docContent' = OfficeNode DocContent  n1 odt1 <> parastyleodt
                parastylename = getParaStyleName parastyle docContent'
                n2' = setAttrVal textStyleNameName parastylename n2 

    TextNode (Span (Just textstyle)) n2 odt2 <> OfficeNode DocContent n1 odt1   = 
        (TextNode (Span Nothing) n2' odt2) <> docContent'
        where   textStyleOdt = toODT textstyle
                docContent' = OfficeNode DocContent  n1 odt1 <> textStyleOdt
                textstylename = getTextStyleName textstyle docContent'
                n2' = setAttrVal textStyleNameName textstylename n2 


    -- APPENDING / PREPENDING TEXT TO OFFICE NODES
    OfficeNode Body n1 EmptyODT <> OfficeNode OfficeTextNode n2 odt2 = 
        OfficeNode Body n1 (OfficeNode OfficeTextNode n2 odt2)

    -- OfficeNode OfficeTextNode <> TextNode
    OfficeNode OfficeTextNode n1 odt1 <> TextNode textType n2 odt2       = 
        OfficeNode OfficeTextNode n1 (odt1 <> TextNode textType n2 odt2)
    TextNode textType n2 odt2 <> OfficeNode OfficeTextNode n1 odt1 = 
        OfficeNode OfficeTextNode n1 (TextNode textType n2 odt2 <> odt1)

    -- OfficeNode OfficeTextNode <> TextLeaf
    OfficeNode OfficeTextNode    n1 odt1 <> TextLeaf typ n2       = 
        OfficeNode OfficeTextNode n1 (odt1 <> TextLeaf typ n2)
    TextLeaf typ n2 <> OfficeNode OfficeTextNode n1 odt1 = 
        OfficeNode OfficeTextNode n1 (TextLeaf typ n2 <> odt1)

    -- TODO: put TextLeaf here
    OfficeNode officeType n1 odt1 <> TextNode textType n2 odt2             = 
        OfficeNode officeType n1 (odt1 <> TextNode textType n2 odt2)
    TextNode textType n2 odt2 <> OfficeNode officeType n1 odt1             = 
        OfficeNode officeType n1 (TextNode textType n2 odt2 <> odt1)
    TextNode textType n1 odt1 <> ODTSeq (OfficeNode OfficeTextNode n2 odt2) odt3 = 
        ODTSeq (OfficeNode OfficeTextNode n2 $ TextNode textType n1 odt1 <> odt2) odt3
    TextNode textType n1 odt1 <> ODTSeq (OfficeNode officeType n2 odt2) odt3 = 
        ODTSeq (OfficeNode officeType n2 odt2) (TextNode textType n1 odt1 <> odt3)

    -- TextLeaf prepend
    TextLeaf typ n2 <> OfficeNode officeType n1 odt1             = 
        OfficeNode officeType n1 (TextLeaf typ n2 <> odt1)
    TextLeaf typ n1 <> ODTSeq (OfficeNode officeType n2 odt2) odt3 = 
        ODTSeq (OfficeNode officeType n2 odt2) (TextLeaf typ n1 <> odt3)

    -- TextLeaf append
    OfficeNode officeType  n1 odt1 <> TextLeaf typ n2            = 
        OfficeNode officeType n1 (odt1 <> TextLeaf typ n2)
    ODTSeq (OfficeNode officeType n2 odt2) odt3 <> TextLeaf typ n1 = 
        ODTSeq (OfficeNode officeType n2 odt2) (odt3 <> TextLeaf typ n1)

    -- APPENDING / PREPENDING TEXT TO TEXT
    -- Insert new text after sequence-decls
    TextNode textType n1 odt1 <> TextNode SequenceDecls n2 odt2        = 
        ODTSeq (TextNode SequenceDecls n2 odt2) (TextNode textType n1 odt1) 
    TextLeaf textType n1 <> TextNode SequenceDecls n2 odt2        = 
        ODTSeq (TextNode SequenceDecls n2 odt2) (TextLeaf textType n1) 


    -- Appending a P to P nodes
    TextNode (P ps1) n1 odt1 <> TextNode (P ps2) n2 odt2             = 
        ODTSeq (TextNode (P ps1) n1 odt1) (TextNode (P ps2) n2 odt2)
    -- Only adds Span into P if there is no style information to get from the document level
    TextNode (P ps) n1 odt1 <> TextNode (Span Nothing) n2 odt2   = 
        TextNode (P ps) n1 (odt1 <> TextNode (Span Nothing) n2 odt2)
    -- -- If there is style information, wrap in ODTSeq so that waits until it
    -- -- meets an element from which it can obtain style information
    -- TextNode    (P ps)      n1 odt1 <> TextNode     (Span textstyle)        n2 odt2 = 
    --     ODTSeq (TextNode (P ps) n1 odt1) (TextNode (Span textstyle) n2 odt2)

    -- Plain text is added straight into the paragraph
    TextNode (P ps) n1 odt1 <> TextLeaf Str n2                  = 
        TextNode (P ps) n1 (odt1 <> TextLeaf Str n2)

    -- Appending / Prepending Str to Span nodes
    TextNode (Span textstyle) n1 odt1 <> TextLeaf Str n = 
        -- TextNode (Span textstyle) n1 (odt1 <> TextLeaf Str n) 
        ODTSeq (TextNode (Span textstyle) n1 odt1) (TextLeaf Str n)
    TextLeaf Str n1 <> TextLeaf Str n2                  = 
        ODTSeq (TextLeaf Str n1) (TextLeaf Str n2)

    -- INTERFACES BETWEEEN SECTIONS
    -- Relevant to mconcat

    -- Interface between scripts and font-face-decls
    OfficeNode Scripts n1 odt1 <> OfficeNode FontFaceDecls n2 odt2 = 
        ODTSeq (OfficeNode Scripts n1 odt1) (OfficeNode FontFaceDecls n2 odt2)

    -- Interface between font-face-decls and automatic-styles
    StyleNode styletype n1 odt1 <> OfficeNode AutoStyles n2 odt2 = 
        ODTSeq (StyleNode styletype n1 odt1) (OfficeNode AutoStyles n2 odt2)
    OfficeNode FontFaceDecls n1 odt1 <> OfficeNode AutoStyles n2 odt2 = 
        ODTSeq (OfficeNode FontFaceDecls n1 odt1) (OfficeNode AutoStyles n2 odt2)

    -- Interface between automatic-styles and body
    StyleNode styletype n1 odt1 <> OfficeNode Body n2 odt2 = 
        ODTSeq (StyleNode styletype n1 odt1) (OfficeNode Body n2 odt2)
    OfficeNode AutoStyles n1 odt1 <> OfficeNode Body n2 odt2 = 
        ODTSeq (OfficeNode AutoStyles n1 odt1) (OfficeNode Body n2 odt2)

    -- MISC NODES
    MiscODT odtxml1 <> MiscODT odtxml2 = ODTSeq (MiscODT odtxml1) (MiscODT odtxml2) 

    -- EMPTY NODES
    EmptyODT <> odt = odt
    odt <> EmptyODT = odt
    
    -- GENERAL RULES
    (ODTSeq odt1 odt2) <> odt3 = ODTSeq odt1 (odt2 <> odt3)
    odt1 <> TextNode (P ps) n odt2 = ODTSeq odt1 (TextNode (P ps) n odt2)
    odt1 <> TextNode (Span textstyle) n odt2 = 
        ODTSeq odt1 (TextNode (Span textstyle) n odt2)
    
    -- TODO: insert a pattern for OfficeNode Text <> TextNode and TextNode OfficeNode

    -- When an ODTSeq is appended to an ODT, each element is appended individually.
    -- This is what allows style names etc. to be imparted to Span nodes before appending
    -- TODO test if this is still needed: in practice appending is done to the document
    odt1 <> ODTSeq odt2 odt3 = (odt1 <> odt2) <> odt3

    -- ERROR: NO PATTERN MATCH
    odt1 <> odt2 = error ("No pattern for \n\t" <> show odt1 <> "\n\t" <> show odt2)

instance Monoid ODT where
    mempty :: ODT
    mempty = EmptyODT

    mappend = (<>)

instance IsList ODT where
    type Item ODT = ODT

    toList :: ODT -> [ODT]
    toList (ODTSeq odt1 odt2) = toList odt1 <> toList odt2
    toList EmptyODT = []
    toList (OfficeNode typ odtxml odt) = OfficeNode typ odtxml EmptyODT : toList odt
    -- Don't unpack spans
    toList (TextNode (Span tstyle) odtxml odt) = [TextNode (Span tstyle) odtxml odt]
    toList (TextNode typ odtxml odt) = TextNode typ odtxml EmptyODT : toList odt
    toList (TextLeaf typ odtxml) = [TextLeaf typ odtxml]
    -- Set StyleType AlwaysInclude to True so that ensures that the style is written
    -- back into the ODT when concatenated in fromList
    toList (StyleNode (StyleType False) odtxml odt) = StyleNode (StyleType True) odtxml EmptyODT : toList odt
    toList (StyleNode typ odtxml odt) = StyleNode typ odtxml EmptyODT : toList odt
    toList (MiscODT odtxml) = [MiscODT odtxml]

    fromList :: [ODT] -> ODT
    fromList = mconcat


-- classes based on ODT

class IsODT a where
    toODT :: a -> ODT

class HasODT a where
    getODT :: a -> ODT
    prependODT :: ODT -> a -> a
    appendODT :: ODT -> a -> a

class HasContentODT a where
    getContentDocODT :: a -> ODT
    replaceContentDocODT :: ODT -> a -> a

class HasStylesODT a where
    getStylesDocODT :: a -> ODT
    replaceStylesDocODT :: ODT -> a -> a
    appendStyleODT :: ODT -> a -> a
    appendStyle :: (IsStyle b, IsODT b) => b -> a -> a

-- TODO: test
instance HasODT ODT where
    getODT = id
    prependODT odt1 odt2 = odt1 <> odt2
    appendODT odt1 odt2 = odt2 <> odt1
 
-- IsODT instances
instance IsODT Node where
    toODT :: Node -> ODT 
    toODT n 
        | hasName       "automatic-styles"  officeNS  n = OfficeNode  AutoStyles n' odt
        | hasName       "body"              officeNS  n = OfficeNode  Body n' odt
        | hasName       "document-content"  officeNS  n = OfficeNode  DocContent n' odt
        | hasName       "document-styles"   officeNS  n = OfficeNode  DocStyles n' odt
        | hasName       "font-face-decls"   officeNS  n = OfficeNode  FontFaceDecls n' odt
        | hasName       "font-face"         styleNS   n = StyleNode   FontFace n' odt
        | hasName       "master-styles"     officeNS  n = OfficeNode  MasterStyles n' odt
        | hasName       "note"              textNS    n = TextNode    Note n' odt
        | hasName       "note-body"         textNS    n = TextNode    NoteBody n' odt
        | hasName       "note-citation"     textNS    n = TextLeaf    NoteCit n'
        | hasLocalName  "NodeContent"                 n = TextLeaf    Str n'
        | hasName       "p"                 textNS    n = TextNode    (P Nothing) n' odt
        | hasName       "scripts"           officeNS  n = OfficeNode  Scripts n' odt
        | hasName       "sequence-decls"    textNS    n = TextNode    SequenceDecls n' odt
        | hasName       "sequence-decl"     textNS    n = TextNode    SequenceDecl n' odt
        | hasName       "span"              textNS    n = TextNode    (Span Nothing) n' odt
        | hasName       "styles"            officeNS  n = OfficeNode  Styles n' odt
        | hasName       "style"             styleNS   n = StyleNode   (StyleType False) n' odt
        | hasName       "text"              officeNS  n = OfficeNode  OfficeTextNode n' odt
        | hasName       "text-properties"   styleNS   n = StyleNode   TextPropsNode n' odt
        | otherwise                                     = MiscODT     (ODTXMLOrig $ n)
        where 
            n' = toODTXML n
            odt = fromNodes (getChildren n)

instance IsODT Element where
    toODT :: Element -> ODT
    toODT e 
        | hasName       "document-content"  officeNS  e = OfficeNode  DocContent  n' odt  
        | hasName       "document-styles"   officeNS  e = OfficeNode  DocStyles   n' odt  
        | otherwise                                     = MiscODT (ODTXMLOrig $ toNode e)
        where 
            n' = toODTXML e
            odt = fromNodes (getChildren e)


-- IsNodes instances
instance IsNodes ODT where
    fromNodes :: [Node] -> ODT
    fromNodes [] = EmptyODT 
    fromNodes (n:[]) = toODT n
    fromNodes (n:ns) = ODTSeq (toODT n) (fromNodes ns)

    toNodes :: ODT -> [Node]
    toNodes (OfficeNode  _ n' odt) = [appendChildren (toNodes odt) . toNode $ n']
    toNodes (StyleNode  _ n' odt) = [appendChildren (toNodes odt) . toNode $ n']
    toNodes (TextNode    _ n' odt) = [appendChildren (toNodes odt) . toNode $ n']
    toNodes (TextLeaf    _ n')     = [toNode n']
    toNodes (MiscODT       n')     = [toNode n']
    toNodes (ODTSeq odt1 odt2)     = toNodes odt1 <> toNodes odt2
    toNodes EmptyODT = []

instance HasAttrs ODT where 
    getAttrs :: ODT -> Map.Map Name T.Text
    getAttrs (OfficeNode _ odtxml _) = getAttrs odtxml
    getAttrs (TextNode _ odtxml _) = getAttrs odtxml
    getAttrs (TextNode _ odtxml _) = getAttrs odtxml
    getAttrs (TextLeaf _ odtxml ) = getAttrs odtxml
    getAttrs (StyleNode _ odtxml _) = getAttrs odtxml
    getAttrs (ODTSeq odt1 odt2) = Map.unions [getAttrs odt1, getAttrs odt2]
    getAttrs (MiscODT odtxml) = Map.empty
    getAttrs EmptyODT = Map.empty

    getAttrVal :: Name -> ODT -> T.Text
    getAttrVal name odt = fromMaybe "" . Map.lookup name . getAttrs $ odt

    hasAttrVal :: Name -> T.Text -> ODT -> Bool
    hasAttrVal name text odt = getAttrVal name odt == text

    setAttrVal :: Name -> T.Text -> ODT -> ODT
    setAttrVal attrname attrval odt = odt -- TODO needs proper treatment

-- Used to realise element attributes as type constructors
-- hasattrs is something that has XML attributes, e.g. ODTXML, or Node
getTypeFromAttr :: (HasAttrs a, IsAttrText b) => Name -> a -> b 
getTypeFromAttr name hasattrs = fromAttrText . getAttrVal name $ hasattrs

instance HasTextStyles ODT where
    -- Returns a list of TextStyles from an ODT
    getTextStyles :: ODT -> [TextStyle]
    getTextStyles odt1
        | Just ts <- toTextStyle autostyles = [ts]
        | ODTSeq odt2 odt3 <- autostyles = getTextStyles odt2 <> getTextStyles odt3
        | EmptyODT <- autostyles = []
        | otherwise = []
        where   autostyles = getTextStylesODT odt1
                stylename = Just . getAttrVal styleNameName $ odt1

    getTextStyleName :: TextStyle -> ODT -> T.Text
    getTextStyleName ts1 odt1 
        | ODTSeq odt2 odt3 <- getTextStylesODT $ odt1 = case toTextStyle odt2 == Just ts1 of
            True -> getAttrVal styleNameName odt2
            False -> getTextStyleName ts1 odt3 

        | StyleNode (StyleType alwaysInclude) n1 textprops <- getTextStylesODT odt1 = 
            case toTextStyle (StyleNode (StyleType alwaysInclude) n1 textprops) == Just ts1 of            
              True -> getAttrVal styleNameName n1
              False -> error $ show $ toTextStyle (StyleNode (StyleType alwaysInclude) n1 textprops)-- show ts1

        | EmptyODT <- getTextStylesODT odt1 = error $ show odt1
        | otherwise = error $ show odt1

    hasTextStyle :: TextStyle -> ODT -> Bool
    hasTextStyle ts1 odt1
        | ODTSeq odt2 odt3 <- getTextStylesODT $ odt1 = case toTextStyle odt2 == Just ts1 of
            True -> True
            False -> hasTextStyle ts1 odt3
        | StyleNode (StyleType alwaysInclude) n1 textprops <- getTextStylesODT odt1 = 
            toTextStyle (StyleNode (StyleType alwaysInclude) n1 textprops) == Just ts1
        | EmptyODT <- getTextStylesODT odt1 = False
        | otherwise = False

instance HasParaStyles ODT where
    -- Returns a list of ParaStyles contained in an ODT
    getParaStyles :: ODT -> [ParaStyle]
    getParaStyles odt1
        | Just ps <- toParaStyle autostyles = [ps]
        | ODTSeq odt2 odt3 <- autostyles = getParaStyles odt2 <> getParaStyles odt3
        | EmptyODT <- autostyles = []
        | otherwise = []
        where   autostyles = getParaStylesODT odt1
                stylename = Just . getAttrVal styleNameName $ odt1

    -- Get the name of a style matching the specification
    -- of ParaStyle from and ODT
    getParaStyleName :: ParaStyle -> ODT -> T.Text
    getParaStyleName ps1 odt1 
        | ODTSeq odt2 odt3 <- getParaStylesODT $ odt1 = case toParaStyle odt2 == Just ps1 of
            True -> getAttrVal styleNameName odt2
            False -> getParaStyleName ps1 odt3 
        | StyleNode (StyleType alwaysInclude) n1 textprops <- getParaStylesODT odt1 = 
            case toParaStyle (StyleNode (StyleType alwaysInclude) n1 textprops) == Just ps1 of
              True -> getAttrVal styleNameName n1
              False -> error $ show odt1
        | EmptyODT <- getParaStylesODT odt1 = error $ show odt1
        | otherwise = error $ show odt1

    -- Return True if the ODT contains a style matching the description
    -- of ParaStyle
    hasParaStyle :: ParaStyle -> ODT -> Bool
    hasParaStyle ps1 odt1
        | ODTSeq odt2 odt3 <- getParaStylesODT odt1 = case toParaStyle odt2 == Just ps1 of
            True -> True
            False -> hasParaStyle ps1 odt3
        | StyleNode (StyleType alwaysInclude) n1 textprops <- getParaStylesODT odt1 = 
            toParaStyle (StyleNode (StyleType alwaysInclude) n1 textprops) == Just ps1
        | EmptyODT <- getParaStylesODT odt1 = False
        | otherwise = False


instance MaybeTextProps ODT where
  toTextProps :: ODT -> Maybe TextProps
  toTextProps (StyleNode TextPropsNode odtxml _) = 
    Just TextProps {
      fontSize = getTypeFromAttr (toName FoNS "font-size") odtxml
    , fontStyle = getTypeFromAttr (toName FoNS "font-style") odtxml
    , fontWeight = getTypeFromAttr (toName FoNS "font-weight") odtxml
    , textUnderline = getTypeFromAttr (toName StyleNS "text-underline-style") odtxml
    , textPosition = getTypeFromAttr (toName StyleNS "text-position") odtxml
  }
  toTextProps _ = Nothing

instance MaybeParaStyle ODT where
  isParaStyle :: ODT -> Bool
  isParaStyle odt 
      | getAttrVal styleFamilyName odt == "paragraph" = True
      | otherwise = False

  -- TODO: make this the same as toTextStyle
  toParaStyle :: ODT -> Maybe ParaStyle
  toParaStyle (StyleNode (StyleType alwaysInclude) n1 odt2) = 
        case isParaStyle odt1 of
            True -> Just ParaStyle {
                          paraTextProps = textprops
                        , paraStyleName = Just . getAttrVal styleNameName $ odt1
                        , parentStyleName = getAttrVal parentStyleNameName $ odt1
                    }
            False -> Nothing

      where odt1 = StyleNode (StyleType alwaysInclude) n1 odt2
            textpropsodt = getTextPropsODT odt2
            textpropsodt'   -- Include this stage to ensure that only one <text-properties> node is returned
              | ODTSeq x y <- textpropsodt = error "More than one <text-properties> node"
              | EmptyODT <- textpropsodt = error "No <text-properties> node"
              | StyleNode TextPropsNode n odt3 <- textpropsodt = textpropsodt
              | otherwise = error "Unknown error"
            textprops 
              | Just tp <- toTextProps textpropsodt = tp
                -- If no <text-properties> node, applies the standard text props
              | Nothing <- toTextProps textpropsodt = newTextProps
              
  toParaStyle _ = Nothing

instance MaybeTextStyle ODT where
  isTextStyle :: ODT -> Bool
  isTextStyle odt 
      | getAttrVal styleFamilyName odt == "text" = True
      | otherwise = False

  toTextStyle :: ODT -> Maybe TextStyle
  toTextStyle odt1
      | StyleNode (StyleType alwaysInclude) n1 (StyleNode TextPropsNode n2 _) <- odt1 = 
          case isTextStyle odt1 of
              True -> Just TextStyle {
                            textTextProps = TextProps {
                                fontSize = getTypeFromAttr foFontSizeName n2
                              , fontStyle = getTypeFromAttr foFsName n2
                              , fontWeight = getTypeFromAttr foFwName n2 
                              , textUnderline = getTypeFromAttr (toName StyleNS "text-underline-style") n2
                              , textPosition = getTypeFromAttr (toName StyleNS "text-position") n2
                            }
                          , textStyleName = Just . getAttrVal styleNameName $ odt1
                      }
              False -> Nothing
      | otherwise = Nothing

-- TODO: complete
instance MaybeStyle ODT where 

  getStyleFamily :: ODT -> Maybe StyleFamily
  getStyleFamily odt 
    | Nothing <- toTextStyle odt = case toParaStyle odt of
        Nothing -> Nothing
        Just parastyle -> Just ParaFamily
    | Just textstyle <- toTextStyle odt = Just TextFamily

  toStyle :: ODT -> Maybe (Either TextStyle ParaStyle)
  toStyle odt 
    | Nothing <- toParaStyle odt = case toTextStyle odt of
        Nothing -> Nothing
        Just textstyle -> Just (Left textstyle)
    | Just parastyle <- toParaStyle odt = Just (Right parastyle)

styleToODT :: (IsStyle a, HasTextProps a) => a -> ODT
styleToODT style = StyleNode (StyleType False) odtxml odt
    where attrs = toStyleAttrMap style
          odtxml = ODTXMLElem (toName StyleNS "style") attrs
          odt = toTextPropsODT style

instance IsODT TextStyle where
  toODT :: TextStyle   ->    ODT
  toODT = styleToODT

instance IsODT ParaStyle where
  toODT :: ParaStyle   ->    ODT
  toODT = styleToODT

getLastODT :: ODT -> ODT
getLastODT (ODTSeq odt1 EmptyODT) = EmptyODT
getLastODT (ODTSeq odt1 odt2) = getLastODT odt2
getLastODT odt = odt

removeLastODT :: ODT -> ODT
removeLastODT (ODTSeq odt1 EmptyODT) = odt1
removeLastODT (ODTSeq odt1 odt2) = ODTSeq odt1 (removeLastODT odt2)
removeLastODT odt = EmptyODT


-------------------------
-- STYLE ODT FUNCTIONS --
-------------------------

-- Returns an ODTSeq (or single instance) of styles
-- i.e. <style:text-properties>
getTextPropsODT :: ODT -> ODT
getTextPropsODT odt1
  | OfficeNode typ n odt2 <- odt1 = getTextPropsODT odt2
  | StyleNode TextPropsNode n odt2 <- odt1 = odt1
  | ODTSeq odt2 odt3 <- odt1 = getTextPropsODT odt2 <> getTextPropsODT odt3
  | otherwise = EmptyODT

-- Returns an ODTSeq (or single instance) of styles
-- i.e. <style:style>
-- Does not return the <office:styles> node
getStylesODT :: ODT -> ODT
getStylesODT odt1
  | OfficeNode typ n odt2 <- odt1 = getStylesODT odt2
  | StyleNode (StyleType alwaysInclude) n odt2 <- odt1 = 
      StyleNode (StyleType alwaysInclude) n odt2
  | ODTSeq odt2 odt3 <- odt1 = getStylesODT odt2 <> getStylesODT odt3
  | otherwise = EmptyODT

-- Returns an ODTSeq (or single instance) of a style node of a specified family
getStyleODTByFamily :: StyleFamily -> ODT -> ODT
getStyleODTByFamily stylefamily odt1 
  | OfficeNode typ n odt2 <- odt1 = getStyleODTByFamily stylefamily odt2
  | StyleNode (StyleType alwaysInclude) n odt2 <- odt1 = case getAttrVal styleFamilyName n of
      stylefamilyAttrVal -> StyleNode (StyleType alwaysInclude) n odt2
      otherwise -> EmptyODT
  | ODTSeq odt2 odt3 <- odt1 = getStyleODTByFamily stylefamily odt2 <> getStyleODTByFamily stylefamily odt3
  | otherwise = EmptyODT
      where stylefamilyAttrVal = toAttrText stylefamily

-- Returns an ODTSeq (or single instance) of text styles
getTextStylesODT :: ODT -> ODT
getTextStylesODT = getStyleODTByFamily TextFamily

-- Returns an ODTSeq (or single instance) of paragraph styles
getParaStylesODT :: ODT -> ODT
getParaStylesODT = getStyleODTByFamily ParaFamily

-- TODO: implement Type Class containing the prefix value of each family
readAutoStyleNameAsInt :: StyleFamily -> [Char] -> Maybe Int
readAutoStyleNameAsInt family s 
  | [] <- s = Nothing
  | ParaFamily <- family = case take 1 s of
      "P" -> Just $ read . drop 1 $ s
      _ -> Nothing
  | TextFamily <- family = case take 1 s of
      "T" -> Just $ read . drop 1 $ s
      _ -> Nothing
  | otherwise = Nothing

-- Convert an automatic text style name to an integer, e.g. "T1" -> 1
readTextStyleNameAsInt :: [Char] -> Maybe Int
readTextStyleNameAsInt = readAutoStyleNameAsInt TextFamily

-- Convert an automatic paragraph style name to an integer, e.g. "P1" -> 1
readParaStyleNameAsInt :: [Char] -> Maybe Int
readParaStyleNameAsInt = readAutoStyleNameAsInt ParaFamily

-- Get the next text style name not already specified
-- TODO change to get maximum
nextTextStyleName :: ODT -> T.Text
nextTextStyleName odt = case length . textStyleNameInts $ odt of 
    0 -> "T1"
    _ -> T.pack $ "T" <> (show . (+ 1) . maximum . textStyleNameInts $ odt)

-- Get the next paragraph style name not already specified
-- TODO: change to get maximum
nextParaStyleName :: ODT -> T.Text
nextParaStyleName odt = case length . paraStyleNameInts $ odt of 
    0 -> "P1"
    _ -> T.pack $ "P" <> (show . (+ 1) . maximum . paraStyleNameInts $ odt)

nextAutoStyleName :: ODT -> ODT -> T.Text
nextAutoStyleName styleODT odt = case getStyleFamily styleODT of
  Just TextFamily -> nextTextStyleName odt
  Just ParaFamily -> nextParaStyleName odt
  Just (MiscFamily _) -> error "No name for Misc style family"
  Nothing -> error $ "No style name for \n" <> show styleODT

-- Takes a sequence of ODTs containing styles, and returns the style name with the highest number
-- This is used for naming styles that don't appear in auto-styles
lastAutoStyleName :: StyleFamily -> ODT -> T.Text
lastAutoStyleName sf odt 
    | StyleNode (StyleType _) n _ <- styles = getAttrVal styleNameName $ n
    | ODTSeq odt1 odt2 <- styles = lastTextStyleName odt2
    | EmptyODT <- styles = ""
    | otherwise = ""
    where styles = getStyleODTByFamily sf odt

lastTextStyleName :: ODT -> T.Text
lastTextStyleName = lastAutoStyleName TextFamily

lastParaStyleName :: ODT -> T.Text
lastParaStyleName = lastAutoStyleName ParaFamily

-- extracts all the int parts of TextStyle names
autoStyleNameInts :: StyleFamily -> ODT -> [Int]
autoStyleNameInts sf odt1 
  | StyleNode (StyleType _) n _ <- styles = 
      catMaybes [readAutoStyleNameAsInt sf . T.unpack . getAttrVal styleNameName $ n]
  | ODTSeq odt2 odt3 <- styles = autoStyleNameInts sf odt2 <> autoStyleNameInts sf odt3
  | EmptyODT <- styles = []
  | otherwise = []
  where styles = getStyleODTByFamily sf odt1
        -- s = 

textStyleNameInts :: ODT -> [Int]
textStyleNameInts = autoStyleNameInts TextFamily

paraStyleNameInts :: ODT -> [Int]
paraStyleNameInts = autoStyleNameInts ParaFamily

toTextPropsODT :: (HasTextProps a) => a -> ODT
toTextPropsODT style = StyleNode TextPropsNode odtxml EmptyODT
    where   attrs = getTextPropsAttrMap style
            odtxml = ODTXMLElem (toName StyleNS "text-properties") attrs

