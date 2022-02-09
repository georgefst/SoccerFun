{-# LANGUAGE UnicodeSyntax, Rank2Types, ExistentialQuantification #-}
module SoccerFun.Types where

import Control.Monad.State
import SoccerFun.Geometry

class NameOf a where nameOf ∷ a → String

data Brain ai memory = Brain {m ∷ memory, ai ∷ ai}

data Half = FirstHalf | SecondHalf deriving (Eq,Show)
type PlayingTime = Minutes

-- | type with an inverse value
class Other a where other ∷ a → a

type TimeUnit = Seconds -- ^ time unit in sec.
type Seconds = Float -- ^ zero < time unit

data Edge = North | South deriving (Eq,Show)

data PlayerID = PlayerID {clubName ∷ ClubName, playerNo ∷ PlayersNumber} deriving (Show,Eq)
type ClubName = String
type PlayersNumber = Int

--thinkS ∷ inp → State (Brain inp out) out
--thinkS i = do
--	Brain m ai ← get
--	let (o, m') = runState (ai i) m
--	put $ Brain m' ai
--	return o

--think ∷ Brain inp out → inp → (Brain inp out, out)
--think (Brain m ai) i = (Brain m' ai, o) where
--	(o, m') = runState (ai i) m


-- | If the referee gives a second yellow he should add red to it himself
data Reprimand = Warning | YellowCard | RedCard deriving (Show, Eq)

data Success = Success | Fail deriving (Show, Eq)

type FramesToGo = Int -- ^ number of frames to go before event ends

data ATeam = Team1 | Team2 deriving (Eq, Show)

type Displacements = [(PlayerID,Displacement)] -- ^ players that need to be displaced
type Displacement = Position -- ^ new position

type ExtraTime = Minutes
type Minutes = Float
