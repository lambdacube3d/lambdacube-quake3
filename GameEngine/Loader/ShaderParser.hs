{-# LANGUAGE OverloadedStrings #-}

module GameEngine.Loader.ShaderParser
  ( parseShaders
  ) where

import Debug.Trace
import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString,pack)
import Text.Megaparsec hiding (count)
import Text.Megaparsec.ByteString
import qualified Text.Megaparsec.Lexer as L

import Text.Show.Pretty (ppShow)

import Data.Char (toLower,isSpace)
import Data.List (foldl')
import LambdaCube.Linear
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Trie as T

import GameEngine.Data.Material

parseShaders :: String -> ByteString -> Either String [(ByteString,CommonAttrs)]
parseShaders fname src = case parse (spaceConsumer' *> many shader <* eof) fname $ BS8.map toLower src of
  Left err  -> Left (parseErrorPretty err)
  Right e   -> Right e

-- q3 shader related parsers
shader :: Parser (ByteString,CommonAttrs)
shader = (\n l -> (n,finishShader $ foldl' (\s f -> f s) defaultCommonAttrs l)) <$>
  line shaderName <* symbol' "{" <*> many shaderAttribute <* symbol' "}"

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

general = try $ choice [cull, deformVertexes, nomipmaps, polygonOffset, portal, skyParms, sort]

stage = (\fl ca -> ca {caStages = (foldl' (\s f -> f s) defaultStageAttrs fl):caStages ca}) <$ symbol' "{" <*> many stageAttribute <* symbol' "}"

stageAttribute :: Parser (StageAttrs -> StageAttrs)
stageAttribute = line $ choice
  [ try $ choice [alphaFunc, alphaGen, animMap, blendFunc, clampMap, depthFunc, depthWrite, mapP, rgbGen, tcGen, tcMod]
  , unknownCommand
  ]

-- utility
waveType = choice
  [ val WT_Sin "sin"
  , val WT_Triangle "triangle"
  , val WT_Square "square"
  , val WT_Sawtooth "sawtooth"
  , val WT_InverseSawtooth "inversesawtooth"
  , val WT_Noise "noise"
  ]
wave = Wave <$> waveType <*> float <*> float <*> float <*> float

skyParms = (\ca -> ca {caIsSky = True}) <$ kw "skyparms" <* (kw "-" <|> (const () <$> word)) <* (kw "-" <|> (const () <$> word)) <* kw "-"

cull = (\a ca -> ca {caCull = a}) <$ kw "cull" <*> choice
  [ val CT_FrontSided "front"
  , val CT_TwoSided "none"
  , val CT_TwoSided "twosided"
  , val CT_TwoSided "disable"
  , val CT_BackSided "backsided"
  , val CT_BackSided "backside"
  , val CT_BackSided "back"
  ]

deformVertexes = (\v ca -> ca {caDeformVertexes = v:caDeformVertexes ca}) <$ kw "deformvertexes" <*> choice
    [ val D_AutoSprite "autosprite"
    , val D_AutoSprite2 "autosprite2"
    , D_Bulge <$ kw "bulge" <*> float <*> float <*> float
    , D_Move <$ kw "move" <*> v3 <*> wave
    , D_Normal <$ kw "normal" <*> float <*> float -- amplitude, frequency
    , val D_ProjectionShadow "projectionshadow"
    , val D_Text0 "text0"
    , val D_Text1 "text1"
    , val D_Text2 "text2"
    , val D_Text3 "text3"
    , val D_Text4 "text4"
    , val D_Text5 "text5"
    , val D_Text6 "text6"
    , val D_Text7 "text7"
    , (\s w -> D_Wave (if s == 0 then 100 else 1/s) w) <$ kw "wave" <*> float <*> wave
    ]
  where
    v3 = V3 <$> float <*> float <*> float

nomipmaps = (\ca -> ca {caNoMipMaps = True}) <$ kw "nomipmaps"
polygonOffset = (\ca -> ca {caPolygonOffset = True}) <$ kw "polygonoffset"
portal = (\ca -> ca {caSort = 1}) <$ kw "portal"

-- sort portal|sky|opaque|banner|underwater|additive|nearest|[number]
sort = (\i ca -> ca {caSort = i}) <$ kw "sort" <*> choice
  [ val 1  "portal"
  , val 2  "sky"
  , val 3  "opaque"
  , val 4  "decal"
  , val 5  "seethrough"
  , val 6  "banner"
  , val 10 "additive"
  , val 16 "nearest"
  , val 8  "underwater"
  , float
  ]

mapP = (\v sa -> sa {saTexture = v}) <$ kw "map" <*> choice
  [ val ST_Lightmap "$lightmap"
  , val ST_WhiteImage "$whiteimage"
  , ST_Map <$> filepath
  ]

clampMap = (\v sa -> sa {saTexture = ST_ClampMap v}) <$> (kw "clampmap" *> filepath)

animMap = (\f v sa -> sa {saTexture = ST_AnimMap f v}) <$ kw "animmap" <*> float <*> some filepath

blendFuncFunc = choice
  [ val (B_One,B_One) "add"
  , val (B_DstColor,B_Zero) "filter"
  , val (B_SrcAlpha,B_OneMinusSrcAlpha) "blend"
  ]

srcBlend = choice
  [ val B_DstAlpha "gl_dst_alpha"
  , val B_DstColor "gl_dst_color"
  , val B_OneMinusDstAlpha "gl_one_minus_dst_alpha"
  , val B_OneMinusDstColor "gl_one_minus_dst_color"
  , val B_OneMinusSrcAlpha "gl_one_minus_src_alpha"
  , val B_One "gl_one"
  , val B_SrcAlphaSaturate "gl_src_alpha_saturate"
  , val B_SrcAlpha "gl_src_alpha"
  , val B_Zero "gl_zero"
  ]

dstBlend = choice
  [ val B_DstAlpha "gl_dst_alpha"
  , val B_OneMinusDstAlpha "gl_one_minus_dst_alpha"
  , val B_OneMinusSrcAlpha "gl_one_minus_src_alpha"
  , val B_OneMinusSrcColor "gl_one_minus_src_color"
  , val B_One "gl_one"
  , val B_SrcAlpha "gl_src_alpha"
  , val B_SrcColor "gl_src_color"
  , val B_Zero "gl_zero"
  ]

blendFunc = (\b sa -> sa {saBlend = Just b, saDepthWrite = dpWr sa}) <$ kw "blendfunc" <*> choice [blendFuncFunc, (,) <$> srcBlend <*> dstBlend]
  where
    dpWr sa = if saDepthMaskExplicit sa then saDepthWrite sa else False

rgbGen = (\v sa -> sa {saRGBGen = v, saAlphaGen = alpha sa v}) <$ kw "rgbgen" <*> choice
  [ RGB_Wave <$ kw "wave" <*> wave
  , RGB_Const <$ kw "const" <* kw "(" <*> float <*> float <*> float <* kw ")"
  , val RGB_Entity "entity"
  , val RGB_ExactVertex "exactvertex"
  , val RGB_IdentityLighting "identitylighting"
  , val RGB_Identity "identity"
  , val RGB_LightingDiffuse "lightingdiffuse"
  , val RGB_OneMinusEntity "oneminusentity"
  , val RGB_OneMinusVertex "oneminusvertex"
  , val RGB_Vertex "vertex"
  ]
  where
    alpha sa v = case v of
        RGB_Vertex  -> case saAlphaGen sa of
            A_Identity  -> A_Vertex
            _           -> saAlphaGen sa
        _           -> saAlphaGen sa

alphaGen = (\v sa -> sa {saAlphaGen = v}) <$ kw "alphagen" <*> choice
  [ A_Wave <$ kw "wave" <*> wave
  , A_Const <$ kw "const" <*> float
  , val A_Entity "entity"
  , val A_Identity "identity"
  , val A_LightingSpecular "lightingspecular"
  , val A_OneMinusEntity "oneminusentity"
  , val A_OneMinusVertex "oneminusvertex"
  , val A_Portal "portal" <* float
  , val A_Vertex "vertex"
  ]

tcGen = (\v sa -> sa {saTCGen = v}) <$ (kw "texgen" <|> kw "tcgen") <*> choice
  [ val TG_Environment "environment"
  , val TG_Lightmap "lightmap"
  , val TG_Base "texture"
  , val TG_Base "base"
  , TG_Vector <$ kw "vector" <*> v3 <*> v3
  ]
  where
    v3 = V3 <$ kw "(" <*> float <*> float <*> float <* kw ")"

tcMod = (\v sa -> sa {saTCMod = v:saTCMod sa}) <$ kw "tcmod" <*> choice
  [ val TM_EntityTranslate "entitytranslate"
  , TM_Rotate <$ kw "rotate" <*> float
  , TM_Scroll <$ kw "scroll" <*> float <*> float
  , TM_Scale <$ kw "scale" <*> float <*> float
  , TM_Stretch <$ kw "stretch" <*> wave
  , TM_Transform <$ kw "transform" <*> float <*> float <*> float <*> float <*> float <*> float
  , TM_Turb <$ kw "turb" <*> float <*> float <*> float <*> float
  ]

depthFunc = (\v sa -> sa {saDepthFunc = v}) <$ kw "depthfunc" <*> (val D_Lequal "lequal" <|> val D_Equal "equal")
depthWrite = (\sa -> sa {saDepthWrite = True, saDepthMaskExplicit = True}) <$ kw "depthwrite"
alphaFunc = (\v sa -> sa {saAlphaFunc = Just v}) <$ kw "alphafunc" <*> (val A_Gt0 "gt0" <|> val A_Lt128 "lt128" <|> val A_Ge128 "ge128")

-- parser primitives
lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

spaceConsumer :: Parser ()
spaceConsumer = L.space (void $ oneOf (" \t" :: String)) lineComment blockComment

spaceConsumer' :: Parser ()
spaceConsumer' = L.space (void spaceChar) lineComment blockComment

line :: Parser a -> Parser a
line p = p <* skipTillEol <* eol <* spaceConsumer'

symbol        = L.symbol spaceConsumer
symbol'       = L.symbol spaceConsumer'
lexeme        = L.lexeme spaceConsumer
lineSymbol    = line . symbol
signedFloat   = realToFrac <$> L.signed spaceConsumer (lexeme floatLiteral) where
  floatLiteral = choice
    [ try L.float
    , try ((read . ("0."++)) <$ char '.' <*> some digitChar)
    , fromIntegral <$> L.integer
    ]

-- aliases
skipRest = spaceConsumer
skip = spaceConsumer
kw = void . symbol
kw' = kw
val v w = const v <$> symbol w
float = signedFloat
word = lexeme (pack <$> some letterChar)
shaderName = lexeme $ pack <$> some (choice [alphaNumChar, oneOf ("/_" :: String)])
filepath = lexeme $ pack <$> some (satisfy $ not . isSpace)

unknownCommand = (\cmd args -> trace ("SKIP1: " ++ cmd ++ args)) <$> some alphaNumChar <*> manyTill anyChar n where n = lookAhead eol

skipTillEol :: Parser String
skipTillEol = do
  let n = lookAhead eol
  cmd <- manyTill anyChar n
  if null cmd then return cmd else trace ("SKIP2: " ++ cmd) $ return cmd

test = do
  let n = "/Users/csaba/games/quake3/unpack/scripts/base.shader2"
  src <- readFile n
  case parseShaders n $ pack src of
    Left  e -> putStrLn e
    Right x -> putStrLn $ ppShow x
