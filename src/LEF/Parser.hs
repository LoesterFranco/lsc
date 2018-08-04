{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LEF.Parser where

import Control.Applicative (optional)
import Control.Monad
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec hiding (option, optional)
import Text.Parsec.String (GenParser)
import Text.Parsec.Combinator hiding (option, optional)
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec.Number
import Prelude hiding (null)

import LEF.Lexer
import LEF.Syntax


type Parser = GenParser (Lexer Token) ()

parseLEF :: Text -> Either ParseError LEF
parseLEF = parse lef [] . lexer []

lef :: Parser LEF
lef = LEF
  <$> many1 option
  <*> many layer
  <*> many via
  <*> many viaRule
  <*> many site
  <*> many1 macro
  <*  endLibrary
  <?> "lef"

option :: Parser Option
option
  =   version
  <|> cases
  <|> bitChars
  <|> divideChars
  <|> units
  <|> useMinSpacing
  <|> clearanceMeasure
  <|> manufacturingGrid
  <?> "option"

version :: Parser Option
version = version_ >> Version <$> double
    <?> "version"

cases :: Parser Option
cases = namescasesensitive_ >> Cases <$> ident
    <?> "cases"

bitChars :: Parser Option
bitChars = busbitchars_ >> BitChars <$> ident
    <?> "bit_chars"

divideChars :: Parser Option
divideChars = dividerchar_ >> DivideChar <$> ident
    <?> "divide_char"

units :: Parser Option
units = units_ >> Units <$> databaseList <* end_ <* units_
    <?> "units"

databaseList :: Parser DatabaseList
databaseList = database_ >> microns_ >> DatabaseList <$> integer
    <?> "database_list"

useMinSpacing :: Parser Option
useMinSpacing = useminspacing_ >> (obs_ <|> pin_) >> UseMinSpacing <$> ident
    <?> "use_min_spacing"

clearanceMeasure :: Parser Option
clearanceMeasure = clearancemeasure_ >> ClearanceMeasure <$> ident
    <?> "clearance_measure"

manufacturingGrid :: Parser Option
manufacturingGrid = manufacturinggrid_ >> ManufacturingGrid <$> double
    <?> "manufacturing_grid"

layer :: Parser Layer
layer = Layer
  <$> layerName
  <*> many layerOption
  <?> "layer"

layerName :: Parser LayerName
layerName = layer_ *> ident <?> "layer_name"

layerOption :: Parser LayerOption
layerOption
  =   Type        <$> (type_        *> ident ) 
  <|> Spacing     <$> (spacing_     *> double)
  <|> Direction   <$> (direction_   *> ident )
  <|> Pitch       <$> (pitch_       *> double)
  <|> Offset      <$> (offset_      *> double)
  <|> Width       <$> (width_       *> double)
  <|> Resistance  <$> (resistance_  *> ident ) <*> double
  <|> Capacitance <$> (capacitance_ *> ident ) <*> double
  <|> EdgeCapacitance <$> (capacitance_ *> double)
  <?> "layer_option"

via :: Parser Via
via = Via
  <$> viaName
  <*> many viaLayer
  <*> (end_ *> ident)
  <?> "via"

viaName :: Parser ViaName
viaName = ViaName <$> ident <*> ident <?> "via_name"

viaLayer :: Parser ViaLayer
viaLayer = ViaLayer
  <$> viaLayerName
  <*> many viaRect
  <?> "via_layer"

viaLayerName :: Parser ViaLayerName
viaLayerName = layer_ *> ident <?> "via_layer_name"

viaRect :: Parser ViaRect
viaRect = rect_ >> ViaRect
  <$> double
  <*> double
  <*> double
  <*> double
  <?> "via_rect"

viaRule :: Parser ViaRule
viaRule = ViaRule
  <$> viaRuleName
  <*> many viaRuleLayer
  <*> (end_ *> ident)
  <?> "via_rule"

viaRuleName :: Parser ViaRuleName
viaRuleName = viarule_ >> ViaRuleName
  <$> ident
  <*> ident
  <?> "via_rule_name"

viaRuleLayer :: Parser ViaRuleLayer
viaRuleLayer = ViaRuleLayer
  <$> viaRuleLayerName
  <*> many viaRuleLayerOption
  <?> "via_rule_layer"

viaRuleLayerName :: Parser ViaRuleLayerName
viaRuleLayerName = layer_ *> ident <?> "via_rule_layer_name"

viaRuleLayerOption :: Parser ViaRuleLayerOption
viaRuleLayerOption
  =   ViaRuleLayerOptionDirection     <$> (direction_ *> ident )
  <|> ViaRuleLayerOptionWidth         <$> (width_     *> double) <*> (to_ *> double)
  <|> ViaRuleLayerOptionSpacing       <$> (spacing_   *> double) <*> (by_ *> double)
  <|> ViaRuleLayerOptionMetalOverhang <$> (metaloverhang_ *> double)
  <|> ViaRuleLayerOptionRect          <$> (rect_ *> double) <*> double <*> double <*> double
  <?> "via_rule_layer_option"

site :: Parser Site
site = Site
  <$> siteName
  <*> many siteOption
  <*> (end_ *> ident)
  <?> "site"

siteName :: Parser SiteName
siteName = site_ *> ident <?> "site_name"

siteOption :: Parser SiteOption
siteOption
  =   SiteClass    <$> (class_    *> ident )
  <|> SiteSymmetry <$> (symmetry_ *> ident ) <*> optional ident
  <|> SiteSize     <$> (size_     *> double) <*> (by_ *> double)
  <?> "site_option"


macro = undefined





endLibrary :: Parser ()
endLibrary = end_ *> library_

--------
----

double :: Parser Double
double = either (fail . show) pure . parse (floating3 False) "double" . T.unpack =<< ident

integer :: Parser Integer
integer = either (fail . show) pure . parse int "integer" . T.unpack =<< ident

maybeToken :: (Token -> Maybe a) -> Parser a
maybeToken test = token showT posT testT
  where
  showT (L _ t) = show t
  posT  (L x _) = pos2sourcePos x
  testT (L _ t) = test t
  pos2sourcePos (l, c) = newPos "" l c

ident :: Parser Ident
ident = maybeToken q
  where q (Tok_Ident t) = Just t
        q _ = Nothing

p :: Token -> Parser ()
p t = maybeToken $ \r -> if r == t then Just () else Nothing
end_ = p Tok_End
library_ = p Tok_Library
version_ = p Tok_Version
capacitance_ = p Tok_Capacitance
resistance_ = p Tok_Resistance
width_ = p Tok_Width
offset_ = p Tok_Offset
pitch_ = p Tok_Pitch
direction_ = p Tok_Direction
spacing_ = p Tok_Spacing
type_ = p Tok_Type
layer_ = p Tok_Layer
units_ = p Tok_Units
dividerchar_ = p Tok_DividerChar
microns_ = p Tok_Microns
database_ = p Tok_Database
busbitchars_ = p Tok_BusBitChars
namescasesensitive_ = p Tok_Namescasesensitive
pin_ = p Tok_Pin
obs_ = p Tok_Obs
useminspacing_ = p Tok_UseMinSpacing
rect_ = p Tok_Rect
metaloverhang_ = p Tok_MetalOverhang
by_ = p Tok_By
to_ = p Tok_To
viarule_ = p Tok_ViaRule
manufacturinggrid_ = p Tok_ManufacturingGrid
clearancemeasure_ = p Tok_ClearanceMeasure
symmetry_ = p Tok_Symmetry
class_ = p Tok_Class
size_ = p Tok_Size
site_ = p Tok_Site
