module GameEngine.Data.MD3 where

import Data.Int
import Data.Map
import Data.Vect hiding (Vector)
import Data.Vector (Vector)
import qualified Data.ByteString.Char8 as SB
import qualified Data.Vector.Storable as SV

data Frame
    = Frame
    { frMins    :: !Vec3
    , frMaxs    :: !Vec3
    , frOrigin  :: !Vec3
    , frRadius  :: !Float
    , frName    :: !SB.ByteString
    } deriving Show

data Tag
    = Tag
    { tgName    :: !SB.ByteString
    , tgOrigin  :: !Vec3
    , tgAxisX   :: !Vec3
    , tgAxisY   :: !Vec3
    , tgAxisZ   :: !Vec3
    } deriving Show

data Shader
    = Shader
    { shName    :: !SB.ByteString
    , shIndex   :: !Int
    } deriving Show

data Surface
    = Surface
    { srName        :: !SB.ByteString
    , srShaders     :: !(Vector Shader)
    , srTriangles   :: !(SV.Vector Int32)
    , srTexCoords   :: !(SV.Vector Vec2)
    , srXyzNormal   :: !(Vector (SV.Vector Vec3,SV.Vector Vec3))
    } deriving Show

data MD3Model
    = MD3Model
    { mdFrames      :: !(Vector Frame)
    , mdTags        :: !(Vector (Map SB.ByteString Tag))
    , mdSurfaces    :: !(Vector Surface)
    } deriving Show
