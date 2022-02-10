{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable #-}
module SoccerFun.Field where

import SoccerFun.Geometry
import SoccerFun.Types (Other (other))
import Data.Typeable

data Field = Field
    {fwidth ∷ FieldWidth, -- ^ width of ball field (64m <=width <=75m)
     flength ∷ FieldLength -- ^ height of ball field (100m<=height<=110m)
    } deriving (Show, Typeable)

type FieldWidth = Metre
type FieldLength = Metre

defaultField ∷ Field
defaultField = Field { fwidth = 75.0, flength = 110.0 }

inPenaltyArea ∷ Field → Home → Position → Bool
inPenaltyArea field home pos = northEdge <= py pos && py pos <= southEdge && if home == West then px pos <= westEdge else px pos >= eastEdge where
    northEdge = fwidth field / 2 - radiusPenaltyArea
    southEdge = fwidth field / 2 + radiusPenaltyArea
    westEdge  = penaltyAreaDepth
    eastEdge  = flength field - penaltyAreaDepth

data Home = West | East deriving (Eq,Show,Typeable)

instance Other Home where
    other West = East
    other East = West

isWest ∷ Home → Bool
isWest West = True
isWest _ = False

isEast ∷ Home → Bool
isEast East = True
isEast _ = False

centre ∷ Field → Position
centre f = Position (flength f / 2) (fwidth f / 2)

-- | goalPoles yields the py coordinates of the north pole and south pole of the goal (note that north < south).
goalPoles ∷ Field → (Metre,Metre)
goalPoles field = (northPole,southPole) where
    northPole = ((fwidth field)-goalWidth)/2.0
    southPole = northPole + goalWidth

-- | Official metrics of a ball field:
radiusCentreCircle = 9.15 ∷ Float
goalWidth = 7.32 :: Float
goalHeight = 2.44 :: Float
goalAreaDepth = 5.50 :: Float
radiusCornerKickArea = 0.90 :: Float

-- | not official, taken for rendering. TODO: remove these things from the module
goalPoleWidth = 0.4 :: Float
radiusCentreSpot = 0.3 :: Float
radiusPenaltySpot = 0.2 ∷ Metre

-- | The vertical size of the penalty area divided by two
radiusPenaltyArea ∷ Metre
radiusPenaltyArea = 20.16

-- | The horizontal size of the penalty area
penaltyAreaDepth ∷ Metre
penaltyAreaDepth = 16.50

penaltySpotDepth = 11.00 ∷ Metre
