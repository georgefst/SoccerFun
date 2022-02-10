{-# LANGUAGE UnicodeSyntax, Rank2Types, ExistentialQuantification, DeriveDataTypeable #-}
-- | This module defines the part of the SoccerFun API that is concerned with the player data types.
module SoccerFun.Player where

import SoccerFun.RefereeAction
import SoccerFun.Prelude
import SoccerFun.Ball
import SoccerFun.Geometry
import SoccerFun.Types
import Control.Monad.State
import SoccerFun.Field
import Data.List (find)
import Data.Typeable

data Player = ∀m. Player
    {playerID ∷ PlayerID,         -- ^ must be unique
     name ∷ String,               -- ^ need not be unique
     height ∷ Length,             -- ^ should be in range [minHeight..maxHeight]
     pos ∷ Position,              -- ^ should be on the ball field
     speed ∷ Speed,               -- ^ absolute direction and velocity with which player is moving
     nose ∷ Angle,                -- ^ absolute direction in which player is looking
     skills ∷ MajorSkills,        -- ^ these improve performance of affected actions
     effect ∷ Maybe PlayerEffect, -- ^ The effect(s) of the previous action
     stamina ∷ Stamina,           -- ^ current stamina of a player: 1.0 is optimal, 0.0 is worst
     health ∷ Health,             -- ^ current health of a player: 1.0 is optimal, 0.0 is worst
     brain ∷ Brain (PlayerAI m) m -- ^ The precious asset: use and update the memory and make decisions
    } deriving Typeable

instance Eq Player where f1 == f2 = playerID f1 == playerID f2
instance Show Player where show (Player {playerID = pid}) = show pid

type PlayerAI memory = BrainInput → State memory PlayerAction

data BrainInput = BrainInput
    {referee ∷ [RefereeAction], -- ^ the referee actions
     ball    ∷ BallState,       -- ^ the state of the ball
     others    ∷ [Player],        -- ^ all other ball players
     me        ∷ Player           -- ^ the player himself
    }

type PlayerWithAction = (PlayerAction, PlayerID)
type PlayerWithEffect = (Maybe PlayerEffect, PlayerID)

type MajorSkills = (Skill,Skill,Skill)
data Skill
    = Running        -- ^ Faster running without ball in possession
    | Dribbling      -- ^ Faster running with ball in possession
    | Rotating       -- ^ Wider range of rotation
    | Gaining        -- ^ Better ball gaining ability
    | Kicking        -- ^ More accurate and wider ball kicking
    | Heading        -- ^ More accurate and wider ball heading
    | Feinting       -- ^ Wider range of feint manouvre
    | Jumping        -- ^ Further jumping
    | Catching       -- ^ Better catching
    | Tackling       -- ^ More effective tackling
    | Schwalbing     -- ^ Better acting of tackles
    | PlayingTheater -- ^ Better acting of playing theater
    deriving (Eq,Show)

data FeintDirection = FeintLeft | FeintRight deriving (Eq, Show)

-- | actions a player can intend to perform
data PlayerAction
    = Move Speed Angle         -- ^ wish to rotate over given angle, and then move with given speed
    | Feint FeintDirection     -- ^ wish to make feint manouvre
    | KickBall Speed3D         -- ^ wish to kick ball with given speed
    | HeadBall Speed3D         -- ^ wish to head ball with given speed
    | GainBall                 -- ^ wish to gain possession of the ball from other player
    | CatchBall                -- ^ wish to catch the ball with his hands
    | Tackle PlayerID Velocity -- ^ wish to tackle identified player, higher velocity is higher chance of succes AND injury (and foul?)
    | Schwalbe                 -- ^ wish to fall as if he was tackled
    | PlayTheater              -- ^ wish to act as if he was hurt
    deriving (Eq,Show)

data PlayerEffect = Moved Speed Angle  -- ^ player has rotated with given angle, and then ran with given speed
    | Feinted FeintDirection            -- ^ player had feinted
    | KickedBall (Maybe Speed3D)        -- ^ player kicked ball (Just v) with velocity, or didn't (Nothing)
    | HeadedBall (Maybe Speed3D)        -- ^ player headed ball (Just v) with velocity, or didn't (Nothing)
    | GainedBall Success                -- ^ player attempt to gain ball from other player
    | CaughtBall Success                -- ^ player caught the ball with his hands
    | Tackled PlayerID Velocity Success -- ^ player attempt to tackle an opponent
    | Schwalbed                         -- ^ player had performed a schwalbe
    | PlayedTheater                     -- ^ player had started to act hurt
    | OnTheGround FramesToGo            -- ^ tackled by someone else; FramesToGo is the amount of frames that you will be on the ground

type Stamina = Float
type Health = Float

defaultPlayer ∷ PlayerID → Player
defaultPlayer playerID = Player
    {playerID = playerID,
     name = "default",
     height = 1.6,
     pos = zero,
     speed = zero,
     nose = zero,
     skills = (Running, Kicking, Dribbling),
     effect = Nothing,
     stamina = maxStamina,
     health = maxHealth,
     brain = Brain
         {m = error "You need to provide defaultPlayer with a new brain.",
          ai = const $ return $ Move zero zero}}

identifyPlayer ∷ PlayerID → Player → Bool
identifyPlayer id fb = id == (playerID fb)

playerIdentity ∷ Player → PlayerID
playerIdentity fb = (playerID fb)

-- | getBall returns the ball (containing its position and speed-information)
-- | that is either free or gained by a player.
-- | For this reason, the list of players must contain all players, otherwise
-- | this function may fail.
getBall ∷ BallState → [Player] → Ball
getBall (Free ball) _ = ball
getBall (GainedBy playerID) allPlayers = case find (identifyPlayer playerID) allPlayers of
    Nothing → error "getBall: no player found with requested identifier."
    Just (Player {pos=pos,speed=speed}) → mkBall pos speed

-- | Returns True if the ball is held by a Keeper in his own penaltyarea
-- | Returns False when the ball is held by a Keeper in open field
-- | Returns False when the ball is not held by a Keeper
-- | Keepers should be numbered with 1.
ballGainedByKeeper ∷ BallState → [Player] → ClubName → Home → Field → Bool
ballGainedByKeeper (Free _) _ _ _ _ = False
ballGainedByKeeper (GainedBy playerID) allPlayers club home field
    = case filter (identifyPlayer playerID) allPlayers of
        [keeper] → playerNo playerID == 1 && inPenaltyArea field (if (clubName playerID==club) then home else (other home)) (pos keeper)
        wrongNumber → error "ballGainedByKeeper: wrong number of keepers found."

clonePlayer ∷ Brain (PlayerAI m) m → Player → Player
clonePlayer brain (Player playerID name height pos speed nose skills effect stamina health _)
    = (Player playerID name height pos speed nose skills effect stamina health brain)

class SameClub a where sameClub ∷ a → a → Bool -- ^ belong to same club

-- TODO: move this to SoccerFun.Geometry
class GetPosition a where getPosition ∷ a → Position

inRadiusOfPlayer ∷ Position → Player → Bool -- ^ True iff position touches/hits player
inRadiusOfPlayer p player = inRadiusOfPosition (zero {pxy=p}) xWidthPlayer yWidthPlayer (height player) (pos player)

skillsAsList ∷ Player → [Skill] -- ^ Skills of the player as a list
skillsAsList fb = (\(a,b,c)→[a,b,c]) (skills fb)

isFirstHalf ∷ Half → Bool
isFirstHalf FirstHalf = True
isFirstHalf _ = False

isSecondHalf ∷ Half → Bool
isSecondHalf SecondHalf = True
isSecondHalf _ = False

-- | chest size of player
xWidthPlayer = 0.7/2.0
-- | stomach size of player
yWidthPlayer = 0.4/2.0

getClubName ∷ Player → ClubName
getClubName fb = nameOf (playerID fb)
isKeeper ∷ Player → Bool
isKeeper fb = playerNo (playerID fb) == 1
isFielder ∷ Player → Bool
isFielder fb = not (isKeeper fb)

-- | minimum length of a person. Advantages:  better gainball; better stamina at sprinting; better dribbling; less health damage when fall, better rotating.
minLength    = 1.6    ∷ Float
-- | maximum length of a person. Advantages:    wider  gainball; better stamina at running;   higher headball;  improved catching; harder kicking.
maxLength    = 2.1    ∷ Float
-- | minimum height of a person. Advantages: better gainball; better stamina at sprinting; better dribbling; less health damage when fall, better rotating.
minHeight = 1.6 ∷ Float
-- | maximum height of a person. Advantages: wider gainball; better stamina at running; higher headball; improved catching; harder kicking.
maxHeight = 2.1 ∷ Float
maxStamina = 1.0 ∷ Float
maxHealth = 1.0 ∷ Float

{-| Player attribute dependent abilities:
        use these functions to make your player correctly dependent of abilities.
-}
maxGainReach ∷ Player → Metre
maxGainReach fb = (if (elem Gaining (skillsAsList fb)) then 0.5 else 0.3) * (height fb)

-- | vertical jumping
maxJumpReach ∷ Player → Metre
maxJumpReach fb = (if (elem Jumping (skillsAsList fb)) then 0.6 else 0.4) * (height fb)

maxGainVelocityDifference ∷ Player → Metre → Velocity
maxGainVelocityDifference fb dPlayerBall = (if (elem Gaining (skillsAsList fb)) then 15.0 else 10.0) - distanceDifficulty where
    distanceDifficulty = max zero (((0.8*(height fb))**4.0)*(dPlayerBall/(height fb)))

maxCatchVelocityDifference ∷ Player → Metre → Velocity
maxCatchVelocityDifference fb dPlayerBall = (if (elem Gaining (skillsAsList fb)) then 20.0 else 17.0) - distanceDifficulty where
    distanceDifficulty = max zero (((0.8*(height fb))**4.0) * (dPlayerBall/(height fb)))

maxKickReach ∷ Player → Metre
maxKickReach fb = (if (elem Kicking (skillsAsList fb)) then 0.6 else 0.4) * (height fb)

maxHeadReach ∷ Player → Metre
maxHeadReach fb = (if (elem Heading (skillsAsList fb)) then 0.4 else 0.2) * (height fb)

-- | includes horizontal jumping
maxCatchReach ∷ Player → Metre
maxCatchReach fb = (if (elem Catching (skillsAsList fb)) then 1.8 else 1.5) * (height fb)

maxTackleReach ∷ Player → Metre
maxTackleReach fb = (if (elem Tackling (skillsAsList fb)) then 0.33 else 0.25) * (height fb)

maxVelocityBallKick ∷ Player → Velocity
maxVelocityBallKick fb = (if (elem Kicking (skillsAsList fb)) then 27.0 else 25.0 + (height fb)/2.0) * (0.2*fatHealth+0.8) where
    fatHealth = getHealthStaminaFactor (health fb) (stamina fb)

maxVelocityBallHead ∷ Player → Velocity → Velocity
maxVelocityBallHead fb ballSpeed = 0.7*ballSpeed + (if (elem Heading (skillsAsList fb)) then 7.0 else 5.0)*(0.1*fatHealth+0.9) where
    fatHealth = getHealthStaminaFactor (health fb) (stamina fb)

maxKickingDeviation ∷ Player → Angle
maxKickingDeviation skills = pi/2.0-- if (elem Kicking skills) (pi/18.0) (pi/2.0)

maxHeadingDeviation ∷ Player → Angle
maxHeadingDeviation skills = pi/4.0-- if (elem Heading skills) (pi/16.0) (pi/5.0)

-- | maximum angle with which player can rotate
maxRotateAngle ∷ Player → Angle
maxRotateAngle fb = pi/18.0*((5.0/(velocity $ speed fb))*(height fb/2.0))

-- | maximum side step of player for feint manouvre
maxFeintStep ∷ Player → Metre
maxFeintStep fb = if (elem Feinting (skillsAsList fb)) then 0.75 else 0.5

-- | combination of stamina and health
type HealthStaminaFactor = Float

getHealthStaminaFactor ∷ Health → Stamina → HealthStaminaFactor
getHealthStaminaFactor health stamina
    | stamina <= health = stamina
    | otherwise = (stamina + health) / 2


teamHome ∷ ATeam → Half → Home
teamHome team half
    | team == Team1 && half == FirstHalf || team == Team2 && half == SecondHalf
                                            = West
    | otherwise = East

opponentHome ∷ ATeam → Half → Home
opponentHome team half
    | team == Team2 && half == FirstHalf || team == Team1 && half == SecondHalf
                                            = West
    | otherwise = East

isMove ∷ PlayerAction → Bool
isMove (Move _ _) = True
isMove _ = False

isGainBall ∷ PlayerAction → Bool
isGainBall GainBall = True
isGainBall _ = False

isCatchBall ∷ PlayerAction → Bool
isCatchBall CatchBall = True
isCatchBall _ = False

isKickBall ∷ PlayerAction → Bool
isKickBall (KickBall _) = True
isKickBall _ = False

isHeadBall ∷ PlayerAction → Bool
isHeadBall (HeadBall _) = True
isHeadBall _ = False

isFeint ∷ PlayerAction → Bool
isFeint (Feint _) = True
isFeint _ = False

isPlayerTackle ∷ PlayerAction → Bool
isPlayerTackle (Tackle _ _) = True
isPlayerTackle _ = False

isSchwalbe ∷ PlayerAction → Bool
isSchwalbe Schwalbe = True
isSchwalbe _ = False

isPlayTheater ∷ PlayerAction → Bool
isPlayTheater PlayTheater = True
isPlayTheater _ = False


isSkillOfAction ∷ Skill → PlayerAction → Bool
isSkillOfAction Running (Move _ _) = True
isSkillOfAction Rotating (Move _ _) = True
isSkillOfAction Gaining GainBall = True
isSkillOfAction Kicking (KickBall _) = True
isSkillOfAction Heading (HeadBall _) = True
isSkillOfAction Feinting (Feint _) = True
isSkillOfAction Tackling (Tackle _ _) = True
isSkillOfAction Schwalbing Schwalbe = True
isSkillOfAction Catching CatchBall = True
isSkillOfAction PlayingTheater PlayTheater = True
isSkillOfAction _ _ = False

isActionOnBall ∷ PlayerAction → Bool
isActionOnBall GainBall = True
isActionOnBall CatchBall = True
isActionOnBall (KickBall _) = True
isActionOnBall (HeadBall _) = True
isActionOnBall _ = False



isMoved ∷ PlayerEffect → Bool
isMoved (Moved _ _) = True
isMoved _ = False

isGainedBall ∷ PlayerEffect → Bool
isGainedBall (GainedBall _) = True
isGainedBall _ = False

isKickedBall ∷ PlayerEffect → Bool
isKickedBall (KickedBall _) = True
isKickedBall _ = False

isHeadedBall ∷ PlayerEffect → Bool
isHeadedBall (HeadedBall _) = True
isHeadedBall _ = False

isFeinted ∷ PlayerEffect → Bool
isFeinted (Feinted _) = True
isFeinted _ = False

isTackled ∷ PlayerEffect → Bool
isTackled (Tackled _ _ _) = True
isTackled _ = False

isSchwalbed ∷ PlayerEffect → Bool
isSchwalbed Schwalbed = True
isSchwalbed _ = False

isCaughtBall ∷ PlayerEffect → Bool
isCaughtBall (CaughtBall _) = True
isCaughtBall _ = False

isPlayedTheater ∷ PlayerEffect → Bool
isPlayedTheater PlayedTheater = True
isPlayedTheater _ = False

isOnTheGround ∷ PlayerEffect → Bool
isOnTheGround (OnTheGround _) = True
isOnTheGround _ = False


failPlayerAction ∷ PlayerAction → PlayerEffect
failPlayerAction (Move s a) = Moved s a
failPlayerAction GainBall = GainedBall Fail
failPlayerAction CatchBall = CaughtBall Fail
failPlayerAction (KickBall v) = KickedBall Nothing
failPlayerAction (HeadBall v) = HeadedBall Nothing
failPlayerAction (Feint d) = Feinted d
failPlayerAction (Tackle p v) = Tackled p v Fail
failPlayerAction Schwalbe = Schwalbed
failPlayerAction PlayTheater = PlayedTheater
--failPlayerAction _ = error "failPlayerAction: unknown action failed"

instance GetPosition Player where getPosition fb = (pos fb)
instance NameOf Player where nameOf fb = name fb
instance NameOf PlayerID where nameOf f = clubName f
instance SameClub PlayerID where sameClub id1 id2 = nameOf id1 == nameOf id2
instance SameClub Player where sameClub fb1 fb2 = sameClub (playerID fb1) (playerID fb2)


{- Player attribute dependent abilities:
-}

{-isReprimanded ∷ PlayerEffect → Bool
isReprimanded (Reprimanded _) = True
isReprimanded _ = False

isScoredGoal ∷ PlayerEffect → Bool
isScoredGoal (ScoredGoal _) = True
isScoredGoal _ = False-}
