{-# LANGUAGE UnicodeSyntax, StandaloneDeriving, DeriveGeneric #-}
-- | record a match to a tape which is serialisable
module SoccerFun.Tape where

import Prelude.Unicode
import Data.Binary
import SoccerFun.MatchControl
import SoccerFun.Player
import SoccerFun.Types
import SoccerFun.Field
import SoccerFun.Geometry
import SoccerFun.Ball
import SoccerFun.RefereeAction
import Control.Monad
import Codec.Compression.GZip
import GHC.Generics

instance Binary Match where
	put m = do
		put $ team1 m
		put $ team2 m
		put $ theBall m
		put $ theField m
		put $ playingHalf m
		put $ playingTime m
		put $ score m
		put $ unittime m
	get = do
		team1 ← get
		team2 ← get
		theBall ← get
		theField ← get
		playingHalf ← get
		playingTime ← get
		score ← get
		unittime ← get
		return $ Match
			{team1       = team1,
	 	 	 team2       = team2,
	 	 	 theBall     = theBall,
	 	 	 theField    = theField,
	 	 	 theReferee  = undefined,
	 	 	 playingHalf = playingHalf,
	 	 	 playingTime = playingTime,
	 	 	 score       = score,
	 	 	 seed        = undefined,
	 	 	 unittime    = unittime}

instance Binary Player where
	put p = do
		put $ playerID p
		put $ name p
		put $ height p
		put $ pos p
		put $ speed p
		put $ nose p
		put $ skills p
		put $ effect p
		put $ stamina p
		put $ health p
	get = do
		playerID ← get
		name ← get
		height ← get
		pos ← get
		speed ← get
		nose ← get
		skills ← get
		effect ← get
		stamina ← get
		health ← get
		return $ Player
			{playerID = playerID,
	 	 	 name = name,
	 	 	 height = height,
	 	 	 pos = pos,
	 	 	 speed = speed,
	 	 	 nose = nose,
	 	 	 skills = skills,
	 	 	 effect = effect,
	 	 	 stamina = stamina,
	 	 	 health = health,
	 	 	 brain = undefined}

deriving instance Generic Half
instance Binary Half
deriving instance Generic Field
instance Binary Field
deriving instance Generic Position3D
instance Binary Position3D
deriving instance Generic Ball
instance Binary Ball
deriving instance Generic BallState
instance Binary BallState
deriving instance Generic PlayerID
instance Binary PlayerID
deriving instance Generic Position
instance Binary Position
deriving instance Generic Speed
instance Binary Speed
deriving instance Generic Skill
instance Binary Skill
deriving instance Generic PlayerEffect
instance Binary PlayerEffect
deriving instance Generic Success
instance Binary Success
deriving instance Generic Speed3D
instance Binary Speed3D
deriving instance Generic FeintDirection
instance Binary FeintDirection
deriving instance Generic RefereeAction
instance Binary RefereeAction
deriving instance Generic PlayerAction
instance Binary PlayerAction
deriving instance Generic Edge
instance Binary Edge
deriving instance Generic ATeam
instance Binary ATeam
deriving instance Generic Reprimand
instance Binary Reprimand


data Tape = Tape [Step]

magic = "SoccerFun tape"
version = "0.4.2"

-- TODO: include framerate
instance Binary Tape where
	put (Tape steps) = do
		put magic
		put version
		put $ compress $ encode steps
	get = do
		let checkMagic m = when (not $ m ≡ magic) (error "This file does not contain a SoccerFun tape!")
		checkMagic =<< get
		let checkVersion v = when (not $ v ≡ version) (error $ "Incompatible tape version: " ⧺ show v)
		checkVersion =<< get
		liftM (Tape ∘ decode ∘ decompress) get

recordMatch ∷ Match → Tape
recordMatch m = Tape $ recordMatch' (([],[]), m) where

	recordMatch' ∷ (([RefereeAction],[PlayerWithAction]),Match) → [(([RefereeAction],[PlayerWithAction]),Match)]
	recordMatch' = takeWhile matchRunning ∘ iterate (stepMatch ∘ snd)

	matchRunning (actions,match) = playingTime match > 0
