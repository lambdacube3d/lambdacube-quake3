module GameEngine.Data.MD3 where

import Data.Int
import Data.Map (Map)
import Data.HashMap.Strict
import Data.Vect hiding (Vector)
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import qualified Data.Vector.Storable as SV

data Frame
    = Frame
    { frMins    :: !Vec3
    , frMaxs    :: !Vec3
    , frOrigin  :: !Vec3
    , frRadius  :: !Float
    , frName    :: !ByteString
    } deriving Show

data Tag
    = Tag
    { tgName    :: !ByteString
    , tgOrigin  :: !Vec3
    , tgAxisX   :: !Vec3
    , tgAxisY   :: !Vec3
    , tgAxisZ   :: !Vec3
    } deriving Show

data Shader
    = Shader
    { shName    :: !ByteString
    , shIndex   :: !Int
    } deriving Show

data Surface
    = Surface
    { srName        :: !ByteString
    , srShaders     :: !(Vector Shader)
    , srTriangles   :: !(SV.Vector Int32)
    , srTexCoords   :: !(SV.Vector Vec2)
    , srXyzNormal   :: !(Vector (SV.Vector Vec3,SV.Vector Vec3))
    } deriving Show

data MD3Model
    = MD3Model
    { mdFrames      :: !(Vector Frame)
    , mdTags        :: !(Vector (HashMap ByteString Tag))
    , mdSurfaces    :: !(Vector Surface)
    } deriving Show

type MD3Skin = Map String String
