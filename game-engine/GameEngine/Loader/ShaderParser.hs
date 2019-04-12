{-# LANGUAGE OverloadedStrings #-}
module GameEngine.Loader.ShaderParser
  ( parseShaders
  ) where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.ByteString.Char8 (ByteString,pack)
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char hiding ()
import qualified Text.Megaparsec.Char.Lexer as L

import Text.Show.Pretty (ppShow)

import Data.Char (toLower,isSpace)
import Data.List (foldl')
import Data.Void
import LambdaCube.Linear
import qualified Data.ByteString.Char8 as BS8

import GameEngine.Data.Material

import Control.Monad.Trans.Writer.Strict

type Parser = WriterT [String] (Parsec (ErrorFancy Void) String)

parseShaders :: String -> String -> Either String ([(String,CommonAttrs)],[String])
parseShaders fname src = case parse (runWriterT $ newlineConsumer *> many shader <* eof) fname $ map toLower src of
  Left err  -> Left $ parseErrorPretty err
  Right a   -> Right a

-- q3 shader related parsers
shader :: Parser (String,CommonAttrs)
shader = (\n l -> (n,finishShader $ foldl' (\s f -> f s) defaultCommonAttrs l)) <$>
  line filepath <* newlineSymbol "{" <*> many shaderAttribute <* newlineSymbol "}"

finishShader :: CommonAttrs -> CommonAttrs
finishShader ca = ca
    { caDeformVertexes  = reverse $ caDeformVertexes ca
    , caStages          = fixedStages
    , caSort            = fixedSort
    }
  where
    -- fix sort value
    srt0        = caSort ca
    srt1        = if caIsSky ca then 2 else srt0
    srt2        = if caPolygonOffset ca && srt1 == 0 then 4 else srt1
    srt3        = snd $ foldl' fixBlendSort (True,srt2) fixedStages
      where
        fixBlendSort (False,s) _ = (False,s)
        fixBlendSort (True,s) sa = case saBlend sa of
            Nothing -> (False,s)
            _       -> let s1 = if s /= 0 then s else if saDepthWrite sa then 5 else 9 in (True,s1)

    srt4        = if srt3 == 0 then 3 else srt3
    fixedSort   = if null fixedStages then 7 else srt4

    fixedStages = reverse $ map fixStage $ caStages ca
    fixStage sa = sa
        { saTCMod       = reverse $ saTCMod sa
        , saTCGen       = tcGen
        , saRGBGen      = rgbGen
        , saBlend       = blend
        , saDepthWrite  = depthWr
        }
      where
        (depthWr,blend) = case saBlend sa of
            Just (B_One,B_Zero) -> (True,Nothing)
            a                   -> (saDepthWrite sa,a)
        rgbGen = case saRGBGen sa of
            RGB_Undefined   -> case saBlend sa of
                Nothing                 -> RGB_IdentityLighting
                Just (B_One,B_SrcAlpha) -> RGB_IdentityLighting
                _                       -> RGB_Identity
            a               -> a
        tcGen = case saTCGen sa of
            TG_Undefined    -> case saTexture sa of
                ST_Lightmap -> TG_Lightmap
                _           -> TG_Base
            a               -> a

shaderAttribute :: Parser (CommonAttrs -> CommonAttrs)
shaderAttribute = choice [line general, stage, line unknownCommand]

general :: Parser (CommonAttrs -> CommonAttrs)
general = try $ choice [cull, deformVertexes, nomipmaps, polygonOffset, portal, skyParms, sort]

stage :: Parser (CommonAttrs -> CommonAttrs)
stage = (\fl ca -> ca {caStages = (foldl' (\s f -> f s) defaultStageAttrs fl):caStages ca}) <$ newlineSymbol "{" <*> many stageAttribute <* newlineSymbol "}"

stageAttribute :: Parser (StageAttrs -> StageAttrs)
stageAttribute = line $ choice
  [ try $ choice [alphaFunc, alphaGen, animMap, blendFunc, clampMap, depthFunc, depthWrite, map_, rgbGen, tcGen, tcMod]
  , unknownCommand
  ]

wave :: Parser Wave
wave = Wave <$> waveType <*> float <*> float <*> float <*> float where
  waveType = choice
    [ value WT_Sin "sin"
    , value WT_Triangle "triangle"
    , value WT_Square "square"
    , value WT_Sawtooth "sawtooth"
    , value WT_InverseSawtooth "inversesawtooth"
    , value WT_Noise "noise"
    ]

skyParms :: Parser (CommonAttrs -> CommonAttrs)
skyParms = (\ca -> ca {caIsSky = True}) <$ symbol "skyparms" <* image <* image <* image
  where image = choice [Nothing <$ symbol "-", Just <$> filepath]

cull :: Parser (CommonAttrs -> CommonAttrs)
cull = (\a ca -> ca {caCull = a}) <$ symbol "cull" <*> choice
  [ value CT_FrontSided "front"
  , value CT_TwoSided "none"
  , value CT_TwoSided "twosided"
  , value CT_TwoSided "disable"
  , value CT_BackSided "backsided"
  , value CT_BackSided "backside"
  , value CT_BackSided "back"
  ]

deformVertexes :: Parser (CommonAttrs -> CommonAttrs)
deformVertexes = (\v ca -> ca {caDeformVertexes = v:caDeformVertexes ca}) <$ symbol "deformvertexes" <*> choice
    [ value D_AutoSprite2 "autosprite2"
    , value D_AutoSprite "autosprite"
    , D_Bulge <$ symbol "bulge" <*> float <*> float <*> float
    , D_Move <$ symbol "move" <*> v3 <*> wave
    , D_Normal <$ symbol "normal" <*> float <*> float -- amplitude, frequency
    , value D_ProjectionShadow "projectionshadow"
    , value D_Text0 "text0"
    , value D_Text1 "text1"
    , value D_Text2 "text2"
    , value D_Text3 "text3"
    , value D_Text4 "text4"
    , value D_Text5 "text5"
    , value D_Text6 "text6"
    , value D_Text7 "text7"
    , (\s w -> D_Wave (if s == 0 then 100 else 1/s) w) <$ symbol "wave" <*> float <*> wave
    ]
  where
    v3 = V3 <$> float <*> float <*> float

nomipmaps :: Parser (CommonAttrs -> CommonAttrs)
nomipmaps = (\ca -> ca {caNoMipMaps = True}) <$ symbol "nomipmaps"

polygonOffset :: Parser (CommonAttrs -> CommonAttrs)
polygonOffset = (\ca -> ca {caPolygonOffset = True}) <$ symbol "polygonoffset"

portal :: Parser (CommonAttrs -> CommonAttrs)
portal = (\ca -> ca {caSort = 1}) <$ symbol "portal"

sort :: Parser (CommonAttrs -> CommonAttrs)
sort = (\i ca -> ca {caSort = i}) <$ symbol "sort" <*> choice
  [ value 1  "portal"
  , value 2  "sky"
  , value 3  "opaque"
  , value 4  "decal"
  , value 5  "seethrough"
  , value 6  "banner"
  , value 10 "additive"
  , value 16 "nearest"
  , value 8  "underwater"
  , float
  ]

map_ :: Parser (StageAttrs -> StageAttrs)
map_ = (\v sa -> sa {saTexture = v}) <$ symbol "map" <*> choice
  [ value ST_Lightmap "$lightmap"
  , value ST_WhiteImage "$whiteimage"
  , ST_Map <$> filepath
  ]

clampMap :: Parser (StageAttrs -> StageAttrs)
clampMap = (\v sa -> sa {saTexture = ST_ClampMap v}) <$> (symbol "clampmap" *> filepath)

animMap :: Parser (StageAttrs -> StageAttrs)
animMap = (\f v sa -> sa {saTexture = ST_AnimMap f v}) <$ symbol "animmap" <*> float <*> some filepath

blendFuncFunc :: Parser (Blending, Blending)
blendFuncFunc = choice
  [ value (B_One,B_One) "add"
  , value (B_DstColor,B_Zero) "filter"
  , value (B_SrcAlpha,B_OneMinusSrcAlpha) "blend"
  ]

srcBlend :: Parser Blending
srcBlend = choice
  [ value B_DstAlpha "gl_dst_alpha"
  , value B_DstColor "gl_dst_color"
  , value B_OneMinusDstAlpha "gl_one_minus_dst_alpha"
  , value B_OneMinusDstColor "gl_one_minus_dst_color"
  , value B_OneMinusSrcAlpha "gl_one_minus_src_alpha"
  , value B_One "gl_one"
  , value B_SrcAlphaSaturate "gl_src_alpha_saturate"
  , value B_SrcAlpha "gl_src_alpha"
  , value B_Zero "gl_zero"
  ]

dstBlend :: Parser Blending
dstBlend = choice
  [ value B_DstAlpha "gl_dst_alpha"
  , value B_OneMinusDstAlpha "gl_one_minus_dst_alpha"
  , value B_OneMinusSrcAlpha "gl_one_minus_src_alpha"
  , value B_OneMinusSrcColor "gl_one_minus_src_color"
  , value B_One "gl_one"
  , value B_SrcAlpha "gl_src_alpha"
  , value B_SrcColor "gl_src_color"
  , value B_Zero "gl_zero"
  ]

blendFunc :: Parser (StageAttrs -> StageAttrs)
blendFunc = (\b sa -> sa {saBlend = Just b, saDepthWrite = dpWr sa}) <$ symbol "blendfunc" <*> choice [blendFuncFunc, (,) <$> srcBlend <*> dstBlend]
  where
    dpWr sa = if saDepthMaskExplicit sa then saDepthWrite sa else False

rgbGen :: Parser (StageAttrs -> StageAttrs)
rgbGen = (\v sa -> sa {saRGBGen = v, saAlphaGen = alpha sa v}) <$ symbol "rgbgen" <*> choice
  [ RGB_Wave <$ symbol "wave" <*> wave
  , RGB_Const <$ symbol "const" <* symbol "(" <*> float <*> float <*> float <* symbol ")"
  , value RGB_Entity "entity"
  , value RGB_ExactVertex "exactvertex"
  , value RGB_IdentityLighting "identitylighting"
  , value RGB_Identity "identity"
  , value RGB_LightingDiffuse "lightingdiffuse"
  , value RGB_OneMinusEntity "oneminusentity"
  , value RGB_OneMinusVertex "oneminusvertex"
  , value RGB_Vertex "vertex"
  ]
  where
    alpha sa v = case v of
        RGB_Vertex  -> case saAlphaGen sa of
            A_Identity  -> A_Vertex
            _           -> saAlphaGen sa
        _           -> saAlphaGen sa

alphaGen :: Parser (StageAttrs -> StageAttrs)
alphaGen = (\v sa -> sa {saAlphaGen = v}) <$ symbol "alphagen" <*> choice
  [ A_Wave <$ symbol "wave" <*> wave
  , A_Const <$ symbol "const" <*> float
  , value A_Entity "entity"
  , value A_Identity "identity"
  , value A_LightingSpecular "lightingspecular"
  , value A_OneMinusEntity "oneminusentity"
  , value A_OneMinusVertex "oneminusvertex"
  , value A_Portal "portal" <* float
  , value A_Vertex "vertex"
  ]

tcGen :: Parser (StageAttrs -> StageAttrs)
tcGen = (\v sa -> sa {saTCGen = v}) <$ (symbol "texgen" <|> symbol "tcgen") <*> choice
  [ value TG_Environment "environment"
  , value TG_Lightmap "lightmap"
  , value TG_Base "texture"
  , value TG_Base "base"
  , TG_Vector <$ symbol "vector" <*> v3 <*> v3
  ]
  where
    v3 = V3 <$ symbol "(" <*> float <*> float <*> float <* symbol ")"

tcMod :: Parser (StageAttrs -> StageAttrs)
tcMod = (\v sa -> sa {saTCMod = v:saTCMod sa}) <$ symbol "tcmod" <*> choice
  [ value TM_EntityTranslate "entitytranslate"
  , TM_Rotate <$ symbol "rotate" <*> float
  , TM_Scroll <$ symbol "scroll" <*> float <*> float
  , TM_Scale <$ symbol "scale" <*> float <*> float
  , TM_Stretch <$ symbol "stretch" <*> wave
  , TM_Transform <$ symbol "transform" <*> float <*> float <*> float <*> float <*> float <*> float
  , TM_Turb <$ symbol "turb" <*> float <*> float <*> float <*> float
  ]

depthFunc :: Parser (StageAttrs -> StageAttrs)
depthFunc = (\v sa -> sa {saDepthFunc = v}) <$ symbol "depthfunc" <*> (value D_Lequal "lequal" <|> value D_Equal "equal")

depthWrite :: Parser (StageAttrs -> StageAttrs)
depthWrite = (\sa -> sa {saDepthWrite = True, saDepthMaskExplicit = True}) <$ symbol "depthwrite"

alphaFunc :: Parser (StageAttrs -> StageAttrs)
alphaFunc = (\v sa -> sa {saAlphaFunc = Just v}) <$ symbol "alphafunc" <*> (value A_Gt0 "gt0" <|> value A_Lt128 "lt128" <|> value A_Ge128 "ge128")

-- parser primitives
lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

spaceConsumer :: Parser ()
spaceConsumer = L.space (void $ oneOf (" \t" :: String)) lineComment blockComment

newlineConsumer :: Parser ()
newlineConsumer = L.space (void spaceChar) lineComment blockComment

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer -- do not consumes line breaks

newlineSymbol :: String -> Parser String
newlineSymbol = L.symbol newlineConsumer -- consumes line breaks

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

float :: Parser Float
float = realToFrac <$> L.signed spaceConsumer (lexeme floatLiteral) where
  floatLiteral = choice
    [ try L.float
    , try ((read . ("0."++)) <$ char '.' <*> some digitChar)
    , fromIntegral <$> L.decimal
    ]

filepath :: Parser String
filepath = lexeme $ some (satisfy $ not . isSpace)

value :: a -> String -> Parser a
value v w = const v <$> symbol w

line :: Parser a -> Parser a
line p = p <* skipTillEol <* newlineConsumer

skipTillEol :: Parser ()
skipTillEol = do
  let n = lookAhead (choice [eol, string "{", string "}"])
  pos <- getPosition
  cmd <- manyTill anyChar n
  unless (null cmd) $ tell ["LEFTOVER - " ++ sourcePosPretty pos ++ ": " ++ cmd]
  return ()

unknownCommand :: Parser (a -> a)
unknownCommand = do
  let n = lookAhead eol
  pos <- getPosition
  cmd <- some alphaNumChar
  args <- manyTill anyChar n
  tell ["IGNORE - " ++ sourcePosPretty pos ++ ": " ++ cmd ++ args]
  return id

-- simple test
test = do
  let n = "/Users/csaba/games/quake3/unpack/scripts/base.shader"
  src <- readFile n
  case parseShaders n $ src of
    Left  e -> putStrLn e
    Right (x,w) -> putStrLn $ ppShow x ++ "\n" ++ unlines w
