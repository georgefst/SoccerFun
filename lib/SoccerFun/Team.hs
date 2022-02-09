{-# LANGUAGE UnicodeSyntax, TypeSynonymInstances, ExistentialQuantification #-}
{-| This module defines the Soccer-Fun API that is concerned with teams.
	All available teams are collected in this module (allAvailableTeams).
-}
module SoccerFun.Team where

import SoccerFun.Player
import SoccerFun.Types
import SoccerFun.Geometry
import SoccerFun.Prelude
import Data.List ((\\), nub, sort)
import SoccerFun.Field

type Team = [Player] -- ^ the fielders are supposed to have different numbers, and all not equal to 1

validateTeam ∷ Team → Team
validateTeam team = map validatePlayer team
	where
--	validatePlayer ∷ Player → Player
	validatePlayer (fb@Player{height=height})
		= fb {height = height `boundedBy` (minHeight,maxHeight)
			  , stamina = maxStamina
			  , health = maxHealth
		  }
replaceInTeam ∷ [Player] → Team → Team
replaceInTeam fbs team = (team \\ fbs) ++ fbs

getTeam ∷ ClubName → [Team] → Team
getTeam cn teams = case [team | team<-teams, nameOf team==cn] of
	(team:_) → team
	_ → error ("Team " ++ show cn ++ " does not seem to exist.\n")

class Mirror a where mirror ∷ Field → a → a

instance NameOf Team where
	nameOf players
		= case players of
			(fb:_) → nameOf (playerID fb)
			none → error "nameOf[Team]: applied to empty team.\n"

isValidTeam ∷ Team → Bool
isValidTeam team = length clubNames == 1
							&&
						  (null keepers || isValidKeeper (head keepers))
							&&
						  all isValidPlayer players
							&&
						  sort (map nrOf players) == sort (nub (map nrOf players))
							&&
						  not (elem 1 (map nrOf fielders))
	where
	(keepers,fielders) = spanfilter isKeeper team
	clubNames = nub (map clubOf players)
	clubName' = head clubNames ∷ String
	players = keepers ++ fielders
	clubOf fb = clubName (playerID fb)
	nrOf fb = playerNo (playerID fb)
	isValidKeeper fb = (playerID fb) == PlayerID {clubName=clubName',playerNo=1}
	isValidPlayer fb = clubOf fb == clubName'

instance Other ATeam where
	other Team1 = Team2
	other Team2 = Team1

instance Mirror a ⇒ Mirror [a] where mirror field as = map (mirror field) as
instance Mirror Player where mirror field fb = fb {pos = mirror field (pos fb)
                                                              , nose = mirror field (nose fb)
                                                              , speed = mirror field (speed fb)
                                                          }
instance Mirror Position where mirror field pos = Position {px = (flength field) - (px pos)
                                                          ,py = (fwidth field) - (py pos)
                                                          }
instance Mirror Speed where mirror field speed = speed {direction = mirror field (direction speed)}
instance Mirror Angle where mirror field angle = if (angle >= pi) then (3.0*pi - angle) else (pi - angle)
