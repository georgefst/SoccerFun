{-# LANGUAGE UnicodeSyntax #-}
module SoccerFun.Ball where

import SoccerFun.Prelude
import System.Random
import SoccerFun.Types
import SoccerFun.Geometry
import SoccerFun.Field

data BallState = Free Ball | GainedBy PlayerID deriving Show

data Ball = Ball {ballPos ∷ Position3D, ballSpeed ∷ Speed3D} deriving Show

{-| mkBall returns a ball with 3D dimensions. -}
mkBall ∷ Position → Speed → Ball
mkBall pos2D speed2D = Ball {ballPos = toPosition3D pos2D, ballSpeed = toSpeed3D speed2D}

{-| ballIsFree yields True iff argument is (Free ...). -}
ballIsFree ∷ BallState → Bool
ballIsFree (Free _) = True
ballIsFree _ = False

{-| ballAtCenter returns a non-moving ball at the center of the ball field. -}
ballAtCenter ∷ Field → Ball
ballAtCenter field = Ball {ballPos=zero {pxy=Position{px=flength field /2.0,py=fwidth field /2.0}}, ballSpeed = zero}

{-| ballIsGainedBy yields True iff the ball is in possession by the given player. -}
ballIsGainedBy ∷ PlayerID → BallState → Bool
ballIsGainedBy id (GainedBy id') = id == id'
ballIsGainedBy _ _ = False

data BounceDirection = Down | Up | Forward | Back

{-| Function used for giving a new random direction towards the given BounceDirection (#param1) -}
bounceBall ∷ BounceDirection → (Speed3D,StdGen) → (Speed3D,StdGen)
bounceBall Up (speed,seed) = let (p,seed1) = random seed in (speed {vz = (10.0-vz speed ) * p}, seed1)
bounceBall Down (speed,seed) = let (p,seed1) = random seed in (speed {vz = vz speed  * p}, seed1)
bounceBall Forward (speed@Speed3D{vxy=sp2d@Speed{direction=d}},seed) = let (p,seed1) = random seed in (speed {vxy = sp2d {direction=d + p*pi/2.0 }},seed1)
bounceBall Back (speed@Speed3D{vxy=sp2d@Speed{direction=d}},seed) = let (p,seed1) = random seed in (speed {vxy = sp2d {direction=d - p*pi/2.0 }},seed1)

------------------------------------------------------------------------------

radiusBall        ∷ Float -- ^ officially it should be 0.113m, but that turns out to be too small for rendering
radiusBall        = 0.4

surfaceResistance    ∷ Float -- ^ maximum speed of ball when moving over surface
surfaceResistance    = 0.85

airResistance        ∷ Float -- ^ maximum speed of ball when moving through air (should depend on velocity)
airResistance        = 0.95

accellerationSec    ∷ Float -- ^ acceleration difference per square second
accellerationSec    = 9.81
