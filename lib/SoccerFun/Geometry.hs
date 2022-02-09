{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances #-}
module SoccerFun.Geometry where

import Prelude.Unicode
import SoccerFun.Prelude
import Data.List (sort)


type Metre = Float
type Length = Metre
type XPos = Metre
type YPos = Metre
type ZPos = Metre

data Position = Position
	{px ∷ XPos, -- ^ x-coordinate in plane (0.0<=px)
	 py ∷ YPos  -- ^ y-coordinate in plane (0.0<=py)
	} deriving (Show,Eq)

data Position3D = Position3D {pxy ∷ Position, pz ∷ ZPos} deriving (Show,Eq)

coordinates ∷ Position3D → (XPos,YPos,ZPos)
coordinates pos = (px $ pxy pos, py $ pxy pos, pz pos)

type XRadius = Metre
type YRadius = Metre
type ZRadius = Metre

-- position3D is within the cube around #param5 with measures of #param2t/m4
inRadiusOfPosition ∷ Position3D → XRadius → YRadius → ZRadius → Position → Bool
inRadiusOfPosition pos1 xr yr zr pos2 = let
		(x1,y1,z1) = coordinates pos1
		Position x2 y2 = pos2
	in x2+xr >= x1 && x1 >= x2-xr && y2+yr >= y1 && y1 >= y2-yr && z1 <= zr

type Angle = Radian -- ^ angle in radians, clockwise
type Radian = Float

angleHowFarFromPi ∷ Angle → Angle
angleHowFarFromPi a = toRadian (if (a' > 180) then (360 - a') else a')
	where
	a' = fromRadian (abs a)

angleHowFarFromAngle ∷ Angle → Angle → Angle
angleHowFarFromAngle a b
	| a' > b' = if a' - b' > 180 then toRadian (b' - a' + 360) else toRadian (a' - b')
	| otherwise =  if b' - a' > 180 then toRadian (a' - b' + 360) else toRadian (b' - a')
	where
	a' = fromRadian (abs a)
	b' = fromRadian (abs b)

-- | @movePoint v p@ moves point p over vector v.
movePoint ∷ RVector → Position → Position
movePoint (RVector dx dy) (Position px py) = Position (px+dx) (py+dy)

movePoint3D ∷ RVector3D → Position3D → Position3D
movePoint3D (RVector3D dxy dz) (Position3D pxy pz) = Position3D (movePoint dxy pxy) (pz+dz)

-- | @pointToRectangle (a,b) c@ returns @c@ if @pointInRectangle (a,b) c@ and the
-- | projected point @c'@ of @c@ that is exactly on the closest edge of rectangle
-- | @(a,b)@.
pointToRectangle ∷ (Position,Position) → Position → Position
pointToRectangle (a,b) c
	| pointInRectangle (a,b) c = c
	| otherwise = let (x,y) = c' in Position x y
	where
	(minX,maxX) = minmax ((px a),(px b))
	(minY,maxY) = minmax ((py a),(py b))
	left = (px c) <= minX
	right = (px c) >= maxX
	above = (py c) <= minY
	below = (py c) >= maxY

	c' | left && above = (minX,minY)
	   | right && above = (maxX,minY)
	   | left && below = (minX,maxY)
	   | right && below = (maxX,maxY)
	   | above = ((px c), minY)
	   | below = ((px c), maxY)
	   | left = (minX,(py c) )
	   | right = (maxX,(py c) )
	   | otherwise = error ("unsuspected error; please rotate with angles between pi and -pi\n")

-- | @pointInRectangle (a,b) c@
-- | returns @True@ iff point @c@ is inside the rectangle determined by
-- | the diagonal corner points @a@ and @b@.
pointInRectangle ∷ (Position,Position) → Position → Bool
pointInRectangle (a,b) c = minX ≤ px c ∧ px c ≤ maxX ∧ minY ≤ py c ∧ py c ≤ maxY where
	(minX,maxX) = minmax ((px a),(px b))
	(minY,maxY) = minmax ((py a),(py b))

inCircleRadiusOfPosition ∷ Position3D → XRadius → ZRadius → Position → Bool
inCircleRadiusOfPosition (Position3D pxy pz) r zr pos
	= dist pxy pos <= r && pz <= zr

data RVector = RVector
	{dx ∷ Metre, -- ^ difference in x-coordinate @|dx| <= 1.0@
	 dy ∷ Metre  -- ^ difference in y-coordinate @|dy| <= 1.0@
	} deriving (Show,Eq)

data RVector3D = RVector3D {dxy ∷ RVector, dz ∷ Metre} deriving (Show,Eq)

-- | speed of an object
data Speed = Speed
	{direction ∷ Angle,   -- ^ direction of object
	 velocity  ∷ Velocity -- ^ velocity of object
	} deriving (Show,Eq)

-- | speed of an object in space
data Speed3D = Speed3D
	{vxy ∷ Speed,   -- ^ surface speed of object
	 vz  ∷ Velocity -- ^ velocity in z-axis (positive: goes up; negative: goes down; 0: horizontally)
	} deriving (Show,Eq)

type Velocity = Float -- ^ velocity in metre/second

class ToSpeed a where toSpeed ∷ a → Speed
class FromSpeed a where fromSpeed ∷ Speed → a
class ToSpeed3D a where toSpeed3D ∷ a → Speed3D
class FromSpeed3D a where fromSpeed3D ∷ Speed3D → a

instance Num RVector3D where
	p1 + p2 = RVector3D {dxy = dxy p1 + dxy p2, dz = dz p1 + dz p2}
	p1 * p2 = RVector3D {dxy = dxy p1 * dxy p2, dz = dz p1 * dz p2}
	p1 - p2 = RVector3D {dxy = dxy p1 - dxy p2, dz = dz p1 - dz p2}
	abs p = RVector3D {dxy = abs $ dxy p, dz = abs $ dz p}
	signum p = RVector3D {dxy = signum $ dxy p, dz = signum $ dz p}
	fromInteger i = RVector3D {dxy = fromInteger i, dz = 0}

instance Num Speed3D where
	p1 + p2 = Speed3D {vxy = vxy p1 + vxy p2, vz = vz p1 + vz p2}
	p1 * p2 = Speed3D {vxy = vxy p1 * vxy p2, vz = vz p1 * vz p2}
	p1 - p2 = Speed3D {vxy = vxy p1 - vxy p2, vz = vz p1 - vz p2}
	abs p = Speed3D {vxy = abs $ vxy p, vz = abs $ vz p}
	signum p = Speed3D {vxy = signum $ vxy p, vz = signum $ vz p}
	fromInteger i = Speed3D {vxy = fromInteger i, vz = 0}

instance Num Position3D where
	p1 + p2 = Position3D {pxy = pxy p1 + pxy p2, pz = pz p1 + pz p2}
	p1 * p2 = Position3D {pxy = pxy p1 * pxy p2, pz = pz p1 * pz p2}
	p1 - p2 = Position3D {pxy = pxy p1 - pxy p2, pz = pz p1 - pz p2}
	abs p = Position3D {pxy = abs $ pxy p, pz = abs $ pz p}
	signum p = Position3D {pxy = signum $ pxy p, pz = signum $ pz p}
	fromInteger i = Position3D {pxy = fromInteger i, pz = 0}

instance Num RVector where
	p1 + p2 = RVector {dx = dx p1 + dx p2, dy = dy p1 + dy p2}
	p1 * p2 = RVector {dx = dx p1 * dx p2, dy = dy p1 * dy p2}
	p1 - p2 = RVector {dx = dx p1 - dx p2, dy = dy p1 - dy p2}
	abs p = RVector {dx = abs $ dx p, dy = abs $ dy p}
	signum p = RVector {dx = signum $ dx p, dy = signum $ dy p}
	fromInteger i = RVector {dx = fromInteger i, dy = 0}

instance Num Position where
	p1 + p2 = Position {px = px p1 + px p2, py = py p1 + py p2}
	p1 * p2 = Position {px = px p1 * px p2, py = py p1 * py p2}
	p1 - p2 = Position {px = px p1 - px p2, py = py p1 - py p2}
	abs p = Position {px = abs $ px p, py = abs $ py p}
	signum p = Position {px = signum $ px p, py = signum $ py p}
	fromInteger i = Position {px = fromInteger i, py = 0}

instance Num Speed where
	p1 + p2 = Speed {direction = direction p1 + direction p2, velocity = velocity p1 + velocity p2}
	p1 * p2 = Speed {direction = direction p1 * direction p2, velocity = velocity p1 * velocity p2}
	p1 - p2 = Speed {direction = direction p1 - direction p2, velocity = velocity p1 - velocity p2}
	abs p = Speed {direction = abs $ direction p, velocity = abs $ velocity p}
	signum p = Speed {direction = signum $ direction p, velocity = signum $ velocity p}
	fromInteger i = Speed {direction = fromInteger i, velocity = 0}

class ToRVector a where toRVector ∷ a → RVector

class ToPosition a where toPosition ∷ a → Position
class FromPosition a where fromPosition ∷ Position → a
class ToPosition3D a where toPosition3D ∷ a → Position3D
class FromPosition3D a where fromPosition3D ∷ Position3D → a


{-| Conversion of radians to degrees and vice versa:
-}
type Degrees = Int -- 0 <= degree < 360 (clockwise)

-- | @scaleVector k {dx,dy}@ returns {k*dx,k*dy}
-- | @scaleVector3D k {dxy,dz}@ returns @{scaleVector k dxy,k*dz}@
scaleVector ∷ Float → RVector → RVector
scaleVector k (RVector dx dy) = RVector (k*dx) (k*dy)

scaleVector3D ∷ Float → RVector3D → RVector3D
scaleVector3D k (RVector3D dxy dz) = RVector3D (scaleVector k dxy) (k*dz)

fromRadian ∷ Radian → Degrees
fromRadian a = (round (a * (360.0 / (2.0*pi)))) `mod` 360

toRadian ∷ Degrees → Radian
toRadian a = (fromIntegral a) * pi / 180.0

-- | @betweenPoints (a,b) c@ returns True iff c is on the line between a and b.
betweenPoints ∷ (Position,Position) → Position → Bool
betweenPoints (a,b) c
	= pointInRectangle (a,b) c && dcx / dcy == dx / dy
	where
	(minX:maxX:_) = sort [(px a),(px b)]
	(minY:maxY:_) = sort [(py a),(py b)]
	(dx, dy) = ((px a) - (px b), (py a) - (py b))
	(dcx,dcy) = ((px a) - (px c), (py a) - (py c))

-- | interpret Float as angle in radians
instance ToRVector Float where toRVector angle = RVector {dx=cos angle,dy=sin angle}
instance ToRVector Position where toRVector p = RVector {dx=(px p),dy=(py p)}

-- | @sizeVector {dx,dy} = sqrt (dx**2 + dy**2)@
-- | @sizeVector3D {dxy,dz} = sqrt ((dx dxy)**2 + (dy dxy)**2 + dz**2)@
sizeVector ∷ RVector → Float
sizeVector (RVector dx dy) = sqrt (dx*82.0 + dy**2.0)

sizeVector3D ∷ RVector3D → Float
sizeVector3D (RVector3D dxy dz) = sqrt ((dx dxy)**2.0 + (dy dxy)**2.0 + dz**2.0)

class Dist a b where dist ∷ a → b → Float
instance Dist Float Float where dist a b = abs (a-b)
instance Dist Position Position where dist a b = sqrt $ (abs $ px a - px b)**2.0 + (abs $ py a - py b)**2.0
instance Dist Position3D Position3D where dist a b = sqrt ((abs (px (pxy a)-px(pxy b)))**2.0 + (abs (py (pxy a)-py (pxy b)))**2.0 + (abs ((pz a)-(pz b)))**2.0)
instance Dist Position Position3D where dist a b = dist (Position3D a 0) b
instance Dist Position3D Position where dist a b = dist a (Position3D b 0)


{-| @orthogonal a@ returns the left- and right- orthogonal angles to a -}
orthogonal ∷ Angle → (Angle,Angle)
orthogonal a = (a+pi/4.0, a-pi/4.0)

instance ToPosition (Float,Float) where toPosition (x,y) = Position {px=x,py=y}
instance ToPosition Position3D where toPosition p3D = pxy p3D
instance FromPosition (Float,Float) where fromPosition p2D = (px p2D, py p2D)
instance FromPosition Position3D where fromPosition p2D = 0 {pxy=p2D}
instance ToPosition3D (Float,Float,Float) where toPosition3D (x,y,z) = Position3D {pxy=toPosition (x,y),pz=z}
instance ToPosition3D Position where toPosition3D p2D = 0 {pxy=p2D}
instance FromPosition3D (Float,Float,Float) where fromPosition3D p3D = (px $ pxy p3D, py $ pxy p3D, pz p3D)
instance FromPosition3D Position where fromPosition3D p3D = pxy p3D
instance ToSpeed Speed3D where toSpeed s = (vxy s)
instance FromSpeed Speed3D where fromSpeed s = 0 {vxy=s}
instance ToSpeed3D Speed where toSpeed3D s = 0 {vxy=s}
instance FromSpeed3D Speed where fromSpeed3D s = (vxy s)

oppositeAngle ∷ Angle → Angle
oppositeAngle a
	| newAngle < (-pi) = newAngle + 2.0*pi
	| otherwise = newAngle
	where
	newAngle = a - pi

angleWithObject ∷ Position → Position → Angle
angleWithObject base target
	| a >= 0.5*pi = if b >= 0 then a - pi else -b
	| otherwise   = if b >= 0 then b - pi else pi + b
--	| a >= 0.5*pi && b >= 0 = ((-pi)+a) -- linksvoor, naar boven, negatieve hoek, -0.5*pi < hoek < 0
--	| a <= 0.5*pi && b >= 0 = (-pi)+b -- linksachter, naar beneden, negatieve hoek, -0.5*pi < hoek < -pi
--	| a <= 0.5*pi && b <= 0 = pi+b -- rechtsachter, naar beneden, positieve hoek, 0.5*pi < hoek < pi
--	| a >= 0.5*pi && b <= 0 = (-b) -- rechtsvoor, naar boven, positieve hoek, 0 < hoek < 0.5*pi
	where
	v = RVector (px base-px target) (py base-py target)
	d = dist base target
	a = acos (max1 ((dx v) / d))
	b = asin (max1 ((dy v) / d))

	max1 ∷ Float → Float
	max1 r
		| r < -1.0 = -1.0
		| r > 1.0 = 1.0
		| otherwise = r

-- | gets the angle between two objects
-- | positive angle is CW, negative is CCW
angleWithObjectForRun ∷ (Position,Angle) → Position → Angle
angleWithObjectForRun (base,angle) target
	| newAngle > pi = newAngle - 2.0*pi
	| newAngle < (-pi) = newAngle + 2.0*pi
	| otherwise = newAngle
	where
	newAngle = angleWithObject base target - angle
