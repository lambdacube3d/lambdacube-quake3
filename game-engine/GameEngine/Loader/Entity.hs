{-# LANGUAGE LambdaCase #-}
module GameEngine.Loader.Entity
  ( parseEntities
  , EntityData(..)
  ) where

import Control.Monad (void)
import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SB8
import Text.Megaparsec hiding (count)
import qualified Text.Megaparsec as L
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L -- (skipLineComment, skipBlockComment, symbol, lexeme, signed)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vect
import Data.Void

type Parser a = Parsec Void String a

data EntityData
  = EntityData
  { classname     :: String
  , spawnflags    :: Int
  , origin        :: Vec3
  , angles        :: Vec3
  , notsingle     :: Bool
  , notteam       :: Bool
  , notfree       :: Bool
  , notq3a        :: Bool
  , speed         :: Maybe Float
  , wait          :: Maybe Float
  , random        :: Maybe Float
  , gravity       :: Maybe Float
  , roll          :: Maybe Float
  , light         :: Maybe Float
  , lip           :: Maybe Float
  , height        :: Maybe Float
  , phase         :: Maybe Float
  , delay         :: Maybe Float
  , color         :: Maybe Vec3
  , count         :: Maybe Int
  , damage        :: Maybe Int
  , nobots        :: Maybe Int
  , nohumans      :: Maybe Int
  , health        :: Maybe Int
  , noglobalsound :: Maybe Int
  , model         :: Maybe String
  , model2        :: Maybe String
  , target        :: Maybe String
  , targetname    :: Maybe String
  , team          :: Maybe String
  , gametype      :: Maybe String
  , message       :: Maybe String
  , noise         :: Maybe String
  , music         :: Maybe String
  , targetShaderName    :: Maybe String
  , targetShaderNewName :: Maybe String
  }
  deriving Show

emptyEntityData = EntityData
  { classname     = ""
  , spawnflags    = 0
  , origin        = zero
  , angles        = zero
  , notsingle     = False
  , notteam       = False
  , notfree       = False
  , notq3a        = False
  , speed         = Nothing
  , wait          = Nothing
  , random        = Nothing
  , gravity       = Nothing
  , roll          = Nothing
  , light         = Nothing
  , lip           = Nothing
  , height        = Nothing
  , phase         = Nothing
  , delay         = Nothing
  , color         = Nothing
  , count         = Nothing
  , damage        = Nothing
  , nobots        = Nothing
  , nohumans      = Nothing
  , health        = Nothing
  , noglobalsound = Nothing
  , model         = Nothing
  , model2        = Nothing
  , target        = Nothing
  , targetname    = Nothing
  , team          = Nothing
  , gametype      = Nothing
  , message       = Nothing
  , noise         = Nothing
  , music         = Nothing
  , targetShaderName    = Nothing
  , targetShaderNewName = Nothing
  }

-- quake 3 entity parser
parseEntities :: String -> String -> Either String [EntityData]
parseEntities fname src = case parse entities fname $ map toLower src of
  Left err  -> Left (parseErrorPretty err)
  Right e   -> Right e

entities :: Parser [EntityData]
entities = newlineConsumer *> many entity <* eof

entity :: Parser EntityData
entity = foldr ($) emptyEntityData <$> between (newlineSymbol "{") (newlineSymbol "}") (some $ choice [line value, line unknownAttribute])

value :: Parser (EntityData -> EntityData)
value = stringLiteral >>= \case
  "classname"   -> (\v e -> e {classname = v}) <$> stringLiteral
  "model"       -> (\v e -> e {model = Just v}) <$> stringLiteral
  "model2"      -> (\v e -> e {model2 = Just v}) <$> stringLiteral
  "target"      -> (\v e -> e {target = Just v}) <$> stringLiteral
  "targetname"  -> (\v e -> e {targetname = Just v}) <$> stringLiteral
  "team"        -> (\v e -> e {team = Just v}) <$> stringLiteral
  "targetshadername"    -> (\v e -> e {targetShaderName = Just v}) <$> stringLiteral
  "targetshadernewname" -> (\v e -> e {targetShaderNewName = Just v}) <$> stringLiteral

  "spawnflags"  -> (\v e -> e {spawnflags = v}) <$> quoted integerLiteral

  "origin"      -> (\v e -> e {origin = v}) <$> quoted vector3
  "angles"      -> (\v e -> e {angles = v}) <$> quoted vector3

  "angle"       -> (\v e -> e {angles = Vec3 0 v 0}) <$> quoted floatLiteral

  "notsingle"   -> (\v e -> e {notsingle = v /= 0}) <$> quoted integerLiteral
  "notteam"     -> (\v e -> e {notteam = v /= 0}) <$> quoted integerLiteral
  "notfree"     -> (\v e -> e {notfree = v /= 0}) <$> quoted integerLiteral
  "notq3a"      -> (\v e -> e {notq3a = v /= 0}) <$> quoted integerLiteral
  "gametype"    -> (\v e -> e {gametype = Just v}) <$> stringLiteral

-- custom; varying defaults
  "message"     -> (\v e -> e {message = Just v}) <$> stringLiteral
  "noise"       -> (\v e -> e {noise = Just v}) <$> stringLiteral
  "music"       -> (\v e -> e {music = Just v}) <$> stringLiteral

  "speed"       -> (\v e -> e {speed = Just v}) <$> quoted floatLiteral
  "wait"        -> (\v e -> e {wait = Just v}) <$> quoted floatLiteral
  "random"      -> (\v e -> e {random = Just v}) <$> quoted floatLiteral
  "gravity"     -> (\v e -> e {gravity = Just v}) <$> quoted floatLiteral
  "roll"        -> (\v e -> e {roll = Just v}) <$> quoted floatLiteral
  "light"       -> (\v e -> e {light = Just v}) <$> quoted floatLiteral
  "lip"         -> (\v e -> e {lip = Just v}) <$> quoted floatLiteral
  "height"      -> (\v e -> e {height = Just v}) <$> quoted floatLiteral
  "phase"       -> (\v e -> e {phase = Just v}) <$> quoted floatLiteral
  "delay"       -> (\v e -> e {delay = Just v}) <$> quoted floatLiteral

  "color"       -> (\v e -> e {color = Just v}) <$> quoted vector3

  "count"       -> (\v e -> e {count = Just v}) <$> quoted integerLiteral
  "dmg"         -> (\v e -> e {damage = Just v}) <$> quoted integerLiteral
  "nobots"      -> (\v e -> e {nobots = Just v}) <$> quoted integerLiteral
  "nohumans"    -> (\v e -> e {nohumans = Just v}) <$> quoted integerLiteral
  "health"      -> (\v e -> e {health = Just v}) <$> quoted integerLiteral
  "noglobalsound" -> (\v e -> e {noglobalsound = Just v}) <$> quoted integerLiteral

  _ -> return id

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

quoted :: Parser a -> Parser a
quoted = between (lexeme $ char '"') (lexeme $ char '"')

stringLiteral :: Parser String
stringLiteral = lexeme $ char '"' >> manyTill anyChar (char '"')

integerLiteral :: Parser Int
integerLiteral = fromIntegral <$> L.signed spaceConsumer (lexeme L.decimal)

floatLiteral :: Parser Float
floatLiteral = realToFrac <$> L.signed spaceConsumer (lexeme float) where
  float = choice
    [ try L.float
    , try ((read . ("0."++)) <$ char '.' <*> some digitChar)
    , fromIntegral <$> L.decimal
    ]

vector3 :: Parser Vec3
vector3 = Vec3 <$> floatLiteral <*> floatLiteral <*> floatLiteral

line :: Parser a -> Parser a
line p = p <* skipTillEol <* newlineConsumer

skipTillEol :: Parser ()
skipTillEol = do
  let n = lookAhead (choice [eol, string "{", string "}"])
  pos <- getPosition
  cmd <- manyTill anyChar n
  --unless (null cmd) $ tell ["LEFTOVER - " ++ sourcePosPretty pos ++ ": " ++ cmd]
  return ()

unknownAttribute :: Parser (a -> a)
unknownAttribute = do
  let n = lookAhead eol
  pos <- getPosition
  cmd <- some alphaNumChar
  args <- manyTill anyChar n
  --tell ["IGNORE - " ++ sourcePosPretty pos ++ ": " ++ cmd ++ args]
  return id
