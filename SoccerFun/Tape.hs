{-# LANGUAGE UnicodeSyntax, TemplateHaskell #-}
-- | record a match to a tape which is serialisable
module SoccerFun.Tape where

import Prelude.Unicode
import Data.DeriveTH
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

$( derive makeBinary ''Half )
$( derive makeBinary ''Field )
$( derive makeBinary ''Position3D )
$( derive makeBinary ''Ball )
$( derive makeBinary ''BallState )
$( derive makeBinary ''PlayerID )
$( derive makeBinary ''Position )
$( derive makeBinary ''Speed )
$( derive makeBinary ''Skill )
$( derive makeBinary ''PlayerEffect )
$( derive makeBinary ''Success )
$( derive makeBinary ''Speed3D )
$( derive makeBinary ''FeintDirection )
$( derive makeBinary ''RefereeAction )
$( derive makeBinary ''PlayerAction )
$( derive makeBinary ''Edge )
$( derive makeBinary ''ATeam )
$( derive makeBinary ''Reprimand )


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
