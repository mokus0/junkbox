module Math.FreeTriangularMesh where

import Math.Angle
import Data.Map
import Data.Vector

data Pt    = Pt     { x,  y  :: !Double } deriving (Eq, Show)
data Basis = Basis  { e0, e1 :: !Pt     } deriving (Eq, Show)
data Triangle = Triangle !Pt !Pt !Pt
    deriving (Eq, Show)

data EdgeID = EdgeID
    { triangleId    :: !Int
    , edgeNum       :: !Int
    } deriving (Eq, Show)

type LinkSrc = EdgeID
data LinkDst = LinkDst
    { edgeId            :: !EdgeID
    , flipsOrientation  :: !EdgeID
    } deriving (Eq, Show)

-- every triangle has its own internal geometry.  There is no global geometry
-- except whatever arises from the triangles and the way they connect.
data Mesh = Mesh
    { triangles :: !(Vector Triangle)
    -- topology; for every edge of every triangle, there are zero or one links.
    -- if there is a link, it is a reference to another triangle edge and a flag
    -- indicating whether orientation is reversed.
    , topology  :: !(Map LinkSrc LinkDst)
    } deriving (Eq, Show)

-- A turtle has a location and velocity, as well as a world matrix
-- which specifies the transformation to apply to objects in the turtle's
-- local triangle when rendering things from that turtle's perspective.
--
-- when a turtle crosses a link, its world matrix changes in such a way that
-- several values are conserved (these might not all be possible at the same
-- time; TODO: work that out):
-- 
--   1) its mass, which is taken to be proportional to the (sqrt of the?)
--      determinant of its world matrix.
--   2) its direction of travel relative to the edge crossed
--   3) speed in its local coordinate system (or in the triangle's native
--      coordinate system?)
--
-- Actually, it may be preferable to make the transformation a property
-- of the link.  One factor is fixed (by the scaling), but perhaps a
-- "formally orthogonal" vector could be specified?
data Turtle = Turtle
    { triangle      :: !Int
    , basis         :: !(Pt, Pt)
    , location      :: !Pt
    , velocity      :: !Angle
    } deriving (Eq, Show)

