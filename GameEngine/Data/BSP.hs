module GameEngine.Data.BSP where

import Data.Word
import Data.Vector (Vector)
import Data.Vect hiding (Vector)

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8

{-
Information:
  http://graphics.stanford.edu/~kekoa/q3/
  http://www.mralligator.com/q3/
-}

data Model
    = Model
    { mdMins         :: !Vec3
    , mdMaxs         :: !Vec3
    , mdFirstSurface :: !Int
    , mdNumSurfaces  :: !Int
    , mdFirstBrush   :: !Int
    , mdNumBrushes   :: !Int
    }

data Shader
    = Shader
    { shName         :: !SB8.ByteString
    , shSurfaceFlags :: !Int
    , shContentFlags :: !Int
    }

data Plane
    = Plane
    { plNormal :: !Vec3
    , plDist   :: !Float
    }

data Node
    = Node
    { ndPlaneNum :: !Int
    , ndChildren :: !(Int,Int)
    , ndMins     :: !Vec3
    , ndMaxs     :: !Vec3
    }

data Leaf
    = Leaf
    { lfCluster          :: !Int
    , lfArea             :: !Int
    , lfMins             :: !Vec3
    , lfMaxs             :: !Vec3
    , lfFirstLeafSurface :: !Int
    , lfNumLeafSurfaces  :: !Int
    , lfFirstLeafBrush   :: !Int
    , lfNumLeafBrushes   :: !Int
    }

data BrushSide
    = BrushSide
    { bsPlaneNum  :: !Int
    , bsShaderNum :: !Int
    }

data Brush
    = Brush
    { brFirstSide :: !Int
    , brNumSides  :: !Int
    , brShaderNum :: !Int
    }

data Fog
    = Fog
    { fgName        :: !SB8.ByteString
    , fgBrushNum    :: !Int
    , fgVisibleSide :: !Int
    }

data DrawVertex
    = DrawVertex
    { dvPosition    :: !Vec3
    , dvDiffuseUV   :: !Vec2
    , dvLightmaptUV :: !Vec2
    , dvNormal      :: !Vec3
    , dvColor       :: !Vec4
    }

data SurfaceType
    = Planar
    | Patch
    | TriangleSoup
    | Flare

data Surface
    = Surface
    { srShaderNum      :: !Int
    , srFogNum         :: !Int
    , srSurfaceType    :: !SurfaceType
    , srFirstVertex    :: !Int
    , srNumVertices    :: !Int
    , srFirstIndex     :: !Int
    , srNumIndices     :: !Int
    , srLightmapNum    :: !Int
    , srLightmapPos    :: !Vec2
    , srLightmapSize   :: !Vec2
    , srLightmapOrigin :: !Vec3
    , srLightmapVec1   :: !Vec3
    , srLightmapVec2   :: !Vec3
    , srLightmapVec3   :: !Vec3
    , srPatchSize      :: !(Int,Int)
    }

data Lightmap
    = Lightmap
    { lmMap :: !SB.ByteString
    }

data LightGrid
    = LightGrid

data Visibility
    = Visibility
    { vsNumVecs     :: !Int
    , vsSizeVecs    :: !Int
    , vsVecs        :: !(Vector Word8)
    }

data BSPLevel
    = BSPLevel
    { blEntities     :: !SB8.ByteString
    , blShaders      :: !(Vector Shader)
    , blPlanes       :: !(Vector Plane)
    , blNodes        :: !(Vector Node)
    , blLeaves       :: !(Vector Leaf)
    , blLeafSurfaces :: !(Vector Int)
    , blLeafBrushes  :: !(Vector Int)
    , blModels       :: !(Vector Model)
    , blBrushes      :: !(Vector Brush)
    , blBrushSides   :: !(Vector BrushSide)
    , blDrawVertices :: !(Vector DrawVertex)
    , blDrawIndices  :: !(Vector Int)
    , blFogs         :: !(Vector Fog)
    , blSurfaces     :: !(Vector Surface)
    , blLightmaps    :: !(Vector Lightmap)
    , blLightgrid    :: !(Vector LightGrid)
    , blVisibility   :: !Visibility
    }
