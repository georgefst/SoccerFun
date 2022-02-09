{-# LANGUAGE UnicodeSyntax, NamedFieldPuns #-}
module SoccerFun.Referee.Ivanov where

import SoccerFun.Prelude
import SoccerFun.Ball
import SoccerFun.Geometry
import SoccerFun.MatchControl
import SoccerFun.Player
import System.Random
import SoccerFun.Referee
import SoccerFun.Team
import SoccerFun.Types
import Data.List
import Control.Monad.Identity
import Data.Maybe
import SoccerFun.Field


chanceOfPenaltySuccess = 0.3
chanceOfSchwalbeSuccess = 0.7
chanceOfTheaterSuccess = 0.8
chanceOfCatchSuccess = 0.5
healthInaccurateFactor = 1.0 -- (is multiplied with 0.5)
nearEventRadius = 10.0
replaceDistance = 5.0

ivanovReferee ∷ Field → Team → Team → Referee
ivanovReferee field t1 t2 = Referee
	{rname = "Ivanov", rbrain = Brain {m = mkIvanovLongTermMemory t1 t2, ai = refBrainIvanov field}}

-- Memory that is passed around for the referee
data IvanovLongTermMemory = IvanovLongTermMemory
	{lastRoundTackles ∷ [TackleAction],
	 inOffsidePosition ∷ [PlayerID],
	 keeper1HadBall ∷ Bool, -- True iff keeper of Team1 had ball in previous round
	 keeper2HadBall ∷ Bool, -- True iff keeper of Team2 had ball in previous round
	 prevHealthPlayers ∷ AssocList PlayerID Health, -- the health of the players in previous round
	 lastKickedTheBall ∷ Maybe PlayerID,
	 ballIsFor ∷ Maybe ATeam,
	 offsidePossible ∷ FreeKickCountdownForOffside, -- because of a certain type of free kick
	 typeOfKickoff ∷ Maybe RefereeAction,
	 waitingForSideSkipping ∷ (Bool,Int),
	 gameLength ∷ Maybe Float,
	 receivedYellow ∷ [PlayerID],
	 initialTeams ∷ (Team,Team)} -- to restore initial positions after kick-off.

type TackleAction = (Victim,Offender,Velocity)
type Victim = (PlayerID,Position,Health) -- Victim of a tackle
type Offender = (PlayerID,Position,Health) -- Offender of a tackle

{- KickForcedByReferee : The next kick should be a kick that is appointed by the referee and therefor can not be offside
	FreeToKick : The first gain or kick after a free kick; can not be offside
	OffsidePossible : It is possible that you are standing offside
-}
data FreeKickCountdownForOffside
	= OffsidePossible
	| FreeToKick
	| KickForcedByReferee
instance Eq FreeKickCountdownForOffside where
	(==) OffsidePossible OffsidePossible = False -- disable offside since it is buggy
	(==) FreeToKick FreeToKick = True
	(==) KickForcedByReferee KickForcedByReferee = True
	(==) _ _ = False

lowerOffsideCounter ∷ IvanovLongTermMemory → IvanovLongTermMemory
lowerOffsideCounter longMem = longMem
	{typeOfKickoff = Nothing,
	 offsidePossible = if (offsidePossible longMem) == KickForcedByReferee then FreeToKick else OffsidePossible}

-- Memory used within one timeslice. Is discarded every round.
data IvanovShortTermMemory = IvanovShortTermMemory
	{gameStoppedForTackle ∷ Bool,
	 punished ∷ [PunishedPlayer],
	 penalty ∷ [(ATeam,Reason)],
	 thisRoundTackles ∷ [TackleAction],
	 myMood ∷ Mood,
	 offsideAndTouchedBall ∷ Maybe PlayerID,
	 ballKickedOrHeaded ∷ Bool}

type PunishedPlayer = (PlayerID,PunishedScore,[Reason])
type PunishedScore = Int -- < 0 points is probably nothing
									-- 1-2 points is probably warning
									-- 3-4 points is probably yellow
									-- > 4 points is probably red

-- Reason of punishments
data Reason = Rtackle | Roffside | Rtheater | Rschwalbe | Rhands | RdangerousPlay | RillegalBallPossession deriving Eq

mkIvanovShortTermMemory ∷ Mood → IvanovShortTermMemory
mkIvanovShortTermMemory m = IvanovShortTermMemory
	{gameStoppedForTackle = False,
	 punished = [],
	 penalty = [],
	 thisRoundTackles = [],
	 myMood = m,
	 offsideAndTouchedBall = Nothing,
	 ballKickedOrHeaded = False}

type Mood = Float -- Random factor used to randomly pick action when Ivanov is not sure about what happened

mkIvanovLongTermMemory ∷ Team → Team → IvanovLongTermMemory
mkIvanovLongTermMemory t1 t2 = IvanovLongTermMemory
	{lastRoundTackles = [],
	 inOffsidePosition = [],
	 keeper1HadBall = False,
	 keeper2HadBall = False,
	 prevHealthPlayers = [],
	 lastKickedTheBall = Nothing,
	 ballIsFor = Just Team1,
	 offsidePossible = OffsidePossible,
	 typeOfKickoff = Just $ CenterKick Team1,
	 waitingForSideSkipping = (False, zero),
	 gameLength = Nothing,
	 receivedYellow = [],
	 initialTeams = (t1,t2)}

-- Has #param2 done something naughty? In other words, is #param2 known in the list provided as #param1?
getNaughtyPlayer ∷ [PunishedPlayer] → PlayerID → Maybe PunishedPlayer
getNaughtyPlayer punished fbID' = case [pun | pun@(fbID,_,_) ← punished, fbID == fbID'] of
	(pun:_) → Just pun
	_ → Nothing

-- Punish baller, if he then was else already punished before, punish him more, otherwise just punish him. Do not make duplicates.
punishMore ∷ Reason → Player → PunishedScore → IvanovShortTermMemory → IvanovShortTermMemory
punishMore reason fb score shortMem = shortMem { punished = newPunished} where
	newPunished = case break' (\(fbID,_,_) → identifyPlayer fbID fb) (punished shortMem) of
						(before,[p],after) → unbreak (before,[addPunishScore reason score p],after)
						(before,[],[]) → before ++ [((playerID fb),score,[reason])]
						otherwise → error "punishMore: short term memory (punished) contains duplicate entries.\n"

-- Give the baller a higher punishscore and add the reason if he then was else not punished before for the same reason
addPunishScore ∷ Reason → PunishedScore → PunishedPlayer → PunishedPlayer
addPunishScore reason newScore (fbID,score,reasons) = (fbID,score+newScore,nub (reasons ++ [reason]))

successfulActions ∷ Team → [PlayerWithEffect]
successfulActions players = map (\Player {effect,playerID} → (effect,playerID)) players

-- How this referee thinks about the match and come to his conclusions.
refBrainIvanov ∷ Field → PlayingTime → TimeUnit → BallState → Half → Team → Team → (IvanovLongTermMemory,StdGen) → ([RefereeAction], (IvanovLongTermMemory,StdGen))
refBrainIvanov field time dt ballState half team1 team2 (longTermMemory,seed) = runIdentity $ do
	longTermMemory ← return $ if isJust (gameLength longTermMemory) then longTermMemory else longTermMemory { gameLength = Just time}
	-- filter actions (what mean actions do I notice and what not)
	(team1Actions,seed) ← return $ filterOutMeanActions allPlayers (successfulActions team1) seed
	-- filter actions team2
	(team2Actions,seed) ← return $ filterOutMeanActions allPlayers (successfulActions team2) seed
	-- look into the consequences of last round tackles
	(p,seed) ← return $ random (seed ∷ StdGen)
	(shortTermMemory, seed) ← return $ analyseTackles (mkIvanovShortTermMemory p) team1Actions team2Actions (lastRoundTackles longTermMemory) seed
	-- analyse the taken actions
	(shortTermMemory,longTermMemory) ← return $ analyseActions Team1 team1Actions shortTermMemory longTermMemory
	(shortTermMemory,longTermMemory) ← return $ analyseActions Team2 team2Actions shortTermMemory longTermMemory

	-- check for other people who are on the ground (for some mysterious reason)
	shortTermMemory ← return $ analysePeopleOnTheGround (receivedYellow longTermMemory) shortTermMemory team1 team2

	-- give end-verdict of this round
	(conclusions,longTermMemory) ← return $ drawConclusions shortTermMemory longTermMemory
	return (conclusions,(longTermMemory,seed))
	where
	allPlayers = team1 ++ team2
	theBall = getBall ballState allPlayers

	kickoffDisplacements ∷ IvanovLongTermMemory → Displacements
	kickoffDisplacements longTermMemory = if half == FirstHalf
		then kickOffTeam1 ++ kickOffTeam2
		else map (\(fbID,pos) → (fbID,mirror field pos)) (kickOffTeam1 ++ kickOffTeam2)
		where
		kickOffTeam1 = [(playerID,pos) | Player {playerID,pos} ← fst (initialTeams longTermMemory)]
		kickOffTeam2 = [(playerID,pos) | Player {playerID,pos} ← snd (initialTeams longTermMemory)]

	-- though teams are returned, only positions AND directions will be altered by a Displace-action
	replacePlayers ∷ Team → Team → RefereeAction → Field → Ball → IvanovLongTermMemory → Displacements
	replacePlayers team1 team2 kickAction field theBall longTermMemory
		| isForKeeper kickAction Team1 = replaceFielders team1 posi ++ replaceTeam team2 posi
		| isForKeeper kickAction Team2 = replaceTeam team1 posi ++ replaceFielders team2 posi
		| isCenterKick kickAction = if ballIsFor longTermMemory == Just Team1
			then replacePlayers' (centre field) radiusCentreCircle team2
			else replacePlayers' (centre field) radiusCentreCircle team1
--		replacePlayers' ∷ Position → Float → [Player] → Displacements
--				kickOffTeam1 = [(playerID,pos) | Player {playerID,pos} ← fst (initialTeams longTermMemory)]
--				kickOffTeam2 = [(playerID,pos) | Player {playerID,pos} ← snd (initialTeams longTermMemory)]
--			in let ng
--				| half == FirstHalf = kickOffTeam1 ++ kickOffTeam2
--				| otherwise = map (\(fbID,pos) → (fbID,mirror field pos)) (kickOffTeam1 ++ kickOffTeam2)
--			in ng
		| isForTeam kickAction Team1 = replaceExceptPlayerCloseToPos team1 posi ++ replaceTeam team2 posi
		| otherwise = replaceTeam team1 posi ++ replaceExceptPlayerCloseToPos team2 posi
		where
		posi = case getKickPos field half kickAction of
														Just p → p
														nothing → (pxy (ballPos theBall))

		replaceTeam ∷ Team → Position → Displacements
		replaceTeam team posi = replacePlayers' posi replaceDistance team

		replaceFielders ∷ Team → Position → Displacements
		replaceFielders team posi = replacePlayers' posi replaceDistance (filter isFielder team)

		replaceExceptPlayerCloseToPos ∷ Team → Position → Displacements
		replaceExceptPlayerCloseToPos team posi = replacePlayers' posi replaceDistance (filter (not . (identifyPlayer (playerID closestPlayer))) team) where closestPlayer = getClosestPlayer team posi (nameOf team)

		isForKeeper ∷ RefereeAction → ATeam → Bool
		isForKeeper (GoalKick team) theTeam = team==theTeam
		isForKeeper _ _ = False

		isForTeam ∷ RefereeAction → ATeam → Bool
		isForTeam (DirectFreeKick team1 _) team2 = team1==team2
		isForTeam (GoalKick team1) team2 = team1==team2
		isForTeam (Corner team1 _) team2 = team1==team2
		isForTeam (ThrowIn team1 _) team2 = team1==team2
		isForTeam (Penalty team1) team2 = team1==team2
		isForTeam (CenterKick team1) team2 = team1==team2
		isForTeam _ _ = True

		getClosestPlayer ∷ [Player] → Position → String → Player
		getClosestPlayer [] posi err = error ("getClosestPlayer: no player to pick from " ++ err)
		getClosestPlayer fbs posi error = foldr1 (isCloser posi) fbs where
			isCloser ∷ Position → Player → Player → Player
			isCloser posi fb1@Player {pos=pos1} fb2@Player {pos=pos2}
				| dist posi pos1 <= dist posi pos2 = fb1
				| otherwise = fb2

		replacePlayers' ∷ Position → Float → [Player] → Displacements
		replacePlayers' posi radius players = [displace (dist (pos fb) posi) fb | fb ← players, dist (pos fb) posi < radius] where
			displace dist fb = ((playerID fb),newPos) where
				newPos = if (dist <= 0.1*radius)
														then (alterPos (movePoint (scaleVector radius RVector {dx=cos (dist*2.0*pi),dy=sin (dist*2.0*pi)}) posi))
														else (alterPos (movePoint (scaleVector radius RVector {dx=cos angle, dy=sin angle}) posi))
				angle = atan (((py (pos fb)) - py posi) / ((px (pos fb)) - px posi))

		-- corrects when players are placed beyond the borders of the field
			alterPos ∷ Position → Position
			alterPos mypos
				| px mypos >= flength field = alterPos' (Just (flength field - one)) Nothing mypos posi
				| (px mypos) <= zero = alterPos' (Just one) Nothing mypos posi
				| (py mypos) >= fwidth field = alterPos' Nothing (Just (fwidth field - one)) mypos posi
				| (py mypos) <= zero = alterPos' Nothing (Just one) mypos posi
				| otherwise = mypos
				where
				alterPos' ∷ (Maybe XPos) → (Maybe YPos) → Position → Position → Position
				alterPos' xpos ypos myPos middlePos
					| isJust xpos && isJust ypos = Position { px = fromJust xpos, py = fromJust ypos }
					| isJust xpos = let amount = abs (fromJust xpos - px myPos) in Position { px = fromJust xpos
													 , py = if py myPos > py middlePos
															 then (if ((py myPos) < fwidth field - amount)
																 then ((py myPos) + amount)
																 else ((py myPos) - amount - 2.0*distY)
															 )
															 else (if ((py myPos) > amount)
																 then ((py myPos) - amount)
																 else ((py myPos) + amount + 2.0*distY)
															 )
													 }
					| isJust ypos = let amount = abs (fromJust ypos - py myPos) in Position { py = fromJust ypos
													 , px = if ((px myPos) > (px middlePos))
															 then (if ((px myPos) + amount < flength field)
																 then ((px myPos) + amount)
																 else ((px myPos) - amount - 2.0*distX)
															 )
															 else (if ((px myPos) > amount)
																 then ((px myPos) - amount)
																 else ((px myPos) + amount + 2.0*distX)
															 )
													 }
					| otherwise = myPos
					where
					distY = abs ((py myPos) - py middlePos)
					distX = abs ((px myPos) - px middlePos)

	analyseTackles ∷ IvanovShortTermMemory → [PlayerWithEffect] → [PlayerWithEffect] → [TackleAction] → StdGen → (IvanovShortTermMemory,StdGen)
	-- were there any tackles in the last round?? NO
	analyseTackles shortTermMemory _ _ [] seed = (shortTermMemory,seed)
	-- YES: there were tackles last round, whistle for that and ignore all other irrelative events
	analyseTackles shortTermMemory team1Actions team2Actions (((victim@PlayerID {playerNo=vNmbr},victimPos,victimHealth), (offender@PlayerID {clubName=oCn,playerNo=oNmbr},offenderPos,offenderHealth), velo) : _) seed = runIdentity $ do -- PA: hmmm, tl of tackles are ignored here.
		offendersTeam ← return $ if nameOf team1 == oCn then Team1 else Team2
		let victimNow = fromMaybe (error "victim not found") $ find (identifyPlayer victim) allPlayers
		let offenderNow = fromMaybe (error "offender not found") $ find (identifyPlayer offender) allPlayers
		shortTermMemory ← return $ shortTermMemory { gameStoppedForTackle = True}
	-- was the ball near the tackle event(s)
		punishScore ← return $ if dist (pxy (ballPos theBall)) offenderPos <= 6.0 then 2 else 1 -- ball was near tackle event(s)
										 +
										 if vNmbr==1 then 5 else (if velo >= 6.0 then 3 else 2) -- penalty for keeper victim is highest
	-- victim is playing theater?
		actionOfVictim ← return $ case concat [maybeToList fa | (fa,fbId) ← if offendersTeam==Team1 then team2Actions else team1Actions, identifyPlayer fbId victimNow] of
											(fa:_) → Just fa
											_ → Nothing
	-- what is the damage to the victim?
		healthDrop ← return $ victimHealth - health victimNow + (p-0.5)* healthInaccurateFactor
	-- is the victim making theater?
		victimPunishScore ← return $ if isJust actionOfVictim then if isPlayedTheater (fromJust actionOfVictim) then 3 else 0 else 0
	-- victim is not playing theater?
		punishScore ← return $ if (victimPunishScore == 0)
											then (definePunishmentOnHealthDrop healthDrop punishScore)
											else punishScore
	-- was the ball on 20% part of the field to the offender's own goal?
		ballWasNearGoal ← return $ half == FirstHalf && oCn == nameOf team1 && ((px (pxy (ballPos theBall))) >= 0.8 * flength field || (px (pxy (ballPos theBall))) <= 0.2 * flength field)
										|| half == SecondHalf && oCn == nameOf team2 && ((px (pxy (ballPos theBall))) >= 0.8 * flength field || (px (pxy (ballPos theBall))) <= 0.2 * flength field)
		ballWasInPenaltyArea ← return $ oCn == nameOf team1 && inPenaltyArea field (opponentHome Team2 half) (pxy (ballPos theBall))
										|| oCn == nameOf team2 && inPenaltyArea field (opponentHome Team1 half) (pxy (ballPos theBall))
		punishScore ← return $ punishScore + if ballWasNearGoal then 1 else 0
		shortTermMemory ← return $ if ballWasInPenaltyArea
											then shortTermMemory {penalty = penalty shortTermMemory ++ [(offendersTeam,Rtackle)]}
											else shortTermMemory
		offenderFromSMemory ← return $ getNaughtyPlayer (punished shortTermMemory) offender
		victimFromSMemory ← return $ getNaughtyPlayer (punished shortTermMemory) victim
		offenderVerdict ← return $ if (isJust offenderFromSMemory)
											then (addPunishScore Rtackle punishScore (fromJust offenderFromSMemory))
											else (offender,punishScore,[Rtackle])
		victimVerdict ← return $ if (isJust victimFromSMemory)
											then (addPunishScore Rtheater victimPunishScore (fromJust victimFromSMemory))
											else (victim,victimPunishScore,[Rtheater])
		return $ if (snd3 offenderVerdict == 0)
			then if (snd3 victimVerdict == 0)
				then (shortTermMemory,seed1)
				else (shortTermMemory { punished = punished shortTermMemory ++ [victimVerdict]},seed1)
			else if (snd3 victimVerdict == 0)
				then (shortTermMemory { punished = punished shortTermMemory ++ [offenderVerdict]},seed1)
				else (shortTermMemory { punished = punished shortTermMemory ++ [offenderVerdict,victimVerdict]},seed1)
		where
		(p,seed1) = random seed

		definePunishmentOnHealthDrop ∷ Float → Int → Int
		definePunishmentOnHealthDrop healthDrop punishScore
			| healthDrop < 0.5 = punishScore
			| 0.5 <= healthDrop && healthDrop <= 1.5 = punishScore + 1
			| 1.5 < healthDrop && healthDrop <= 3.5 = punishScore + 2
			| otherwise = punishScore + 3

	analyseActions ∷ ATeam → [PlayerWithEffect] → IvanovShortTermMemory → IvanovLongTermMemory
										→ (IvanovShortTermMemory,IvanovLongTermMemory)
	analyseActions team [] shortTermMemory longTermMemory = (shortTermMemory,longTermMemory { lastRoundTackles = []})
	analyseActions team (action@(fa,PlayerID {clubName=cn,playerNo=nmbr}):actions) shortTermMemory longTermMemory
	-- Am I in the pause between the first and the second half (long enough to allow switching of sides)
		| fst (waitingForSideSkipping longTermMemory) = runIdentity $ do
			let ng
				| snd (waitingForSideSkipping longTermMemory) <= zero = (shortTermMemory,longTermMemory { waitingForSideSkipping = (False,zero)})
				| otherwise = (shortTermMemory,longTermMemory { waitingForSideSkipping = (True,snd (waitingForSideSkipping longTermMemory)-one)})
			return ng
	-- did I stop the game because of tackle-actions?
		| (gameStoppedForTackle shortTermMemory) = runIdentity $ do
			let ng
			-- No more analysing game, but still look at mean actions with a connection to the tackle(s)
				| perhaps isSchwalbed fa
					= let ng
						| isTackleVictim (playerID taf) (lastRoundTackles longTermMemory) -- was he victim
						|| isTackleOffender (playerID tef) (lastRoundTackles longTermMemory) -- or offender
						|| nearLastRoundTackle (pos fb) (lastRoundTackles longTermMemory) -- or near a tackle?
							= let ng
								| inPenaltyArea field (opponentHome team half) (pos fb) -- was it in penaltyarea?
									= analyseActions team actions shortTermMemory { penalty = penalty shortTermMemory ++ [(team,Rschwalbe)]} longTermMemory
								| otherwise -- punish him or punish him more
									= analyseActions team actions (punishMore Rschwalbe tef 1 shortTermMemory) longTermMemory
							in ng
						| otherwise -- otherwise ignore
							= analyseActions team actions shortTermMemory longTermMemory--ignoreThisAction
					in ng
				| perhaps isPlayedTheater fa
					= let ng
						| isTackleOffender (playerID tef) (lastRoundTackles longTermMemory) -- was he offender
						|| nearLastRoundTackle (pos fb) (lastRoundTackles longTermMemory) -- or near a tackle
						&& not (isTackleVictim (playerID taf) (lastRoundTackles longTermMemory))-- and is not a victim
							= let ng
								| inPenaltyArea field (opponentHome team half) (pos fb) -- was it in penaltyarea?
									= analyseActions team actions shortTermMemory { penalty = penalty shortTermMemory ++ [(team,Rtheater)]} longTermMemory
								| otherwise
									= analyseActions team actions (punishMore Rtheater tef 1 shortTermMemory) longTermMemory
							in ng
					-- victims are already scanned during tackle analyses to see if it then was else worth to check the health of the victim
						| otherwise -- otherwise ignore
							= analyseActions team actions shortTermMemory longTermMemory--ignoreThisAction
					in ng
				| perhaps isTackled fa
					= let ng
						| isTackleVictim (playerID taf) ((lastRoundTackles longTermMemory)) -- was he victim
						|| isTackleOffender (playerID tef) ((lastRoundTackles longTermMemory)) -- or offender
						|| nearLastRoundTackle (pos fb) (lastRoundTackles longTermMemory) -- or near a tackle?
							= let ng
								| inPenaltyArea field (opponentHome team half) (pos fb) -- was it in penaltyarea?
									= analyseActions team actions shortTermMemory { penalty = penalty shortTermMemory ++ [(team,Rschwalbe)]} longTermMemory
								| otherwise -- punish him or punish him more
									= analyseActions team actions (punishMore Rtackle tef 2 shortTermMemory) longTermMemory
							in ng
						--otherwise ignore
					-- Usually ignore, but: a tackle is something serious... when I'm in a bad mood, I will not ignore this one
						| (myMood shortTermMemory) > 0.8
							= let ng
								| inPenaltyArea field (opponentHome team half) (pos fb) -- was it in penaltyarea?
									= analyseActions team actions shortTermMemory { penalty = penalty shortTermMemory ++ [(team,Rschwalbe)]} longTermMemory
								| otherwise
									= analyseActions team actions (punishMore Rtackle tef 3 shortTermMemory) longTermMemory
							in ng
						| otherwise -- player was lucky...
							= analyseActions team actions shortTermMemory longTermMemory--ignoreThisAction
					in ng
				| perhaps isCaughtBall fa
					= let ng
						| isKeeper tef && inKeeperSpot tef team1Name currHome field -- is he a keeper?
							= analyseActions team actions shortTermMemory longTermMemory--ignoreThisAction
						| isTackleVictim (playerID taf) (lastRoundTackles longTermMemory) -- was he victim
						|| isTackleOffender (playerID tef) (lastRoundTackles longTermMemory) -- or offender
						|| nearLastRoundTackle (pos fb) (lastRoundTackles longTermMemory) -- or near a tackle?
							= let ng
								| inPenaltyArea field (opponentHome team half) (pos fb) -- was it in penaltyarea?
									= analyseActions team actions shortTermMemory { penalty = penalty shortTermMemory ++ [(team,Rschwalbe)]} longTermMemory
								| otherwise -- punish him or punish him more
									= analyseActions team actions (punishMore Rhands tef 2 shortTermMemory) longTermMemory
							in ng
						| otherwise -- otheriwse ignore
							= analyseActions team actions shortTermMemory longTermMemory--ignoreThisAction
					in ng
			-- All other kind of actions are ignored
				| otherwise = analyseActions team actions shortTermMemory longTermMemory--ignoreThisAction
			return ng
	-- There were no tackle-actions last round → Ivanov needs to keep thinking
	-- Everyone is free to move over the field
		| perhaps isMoved fa || perhaps isFeinted fa = analyseActions team actions shortTermMemory longTermMemory--ignoreThisAction
	-- Gainball is illegal when in offside position
	-- A legal gainBall causes no one to be in offside position (until a kick- or headball event)
		| perhaps isGainedBall fa = runIdentity $ do
		-- He was not allowed to get the ball (because of referee-action in the previous round)
			if not (wasAllowedToGetBall (ballIsFor longTermMemory) (nameOf team1) (nameOf team2) cn)
				then return $ analyseActions team actions (punishMore RillegalBallPossession tef 1 shortTermMemory) longTermMemory
				else do
					longTermMemory ← return $ longTermMemory { ballIsFor = Nothing}
				-- He was in offside position
					return $ if elem (playerID tef) (inOffsidePosition longTermMemory) && (offsidePossible longTermMemory) == OffsidePossible
					-- Detect offside for wistle
						then analyseActions team actions (punishMore Roffside tef 0 shortTermMemory) { offsideAndTouchedBall=Just (playerID tef)} longTermMemory
						else analyseActions team actions shortTermMemory longTermMemory { inOffsidePosition = []}
	-- {Kick/Head}Ball is illegal when in offside position (ball was not gained)
		| perhaps isKickedBall fa || perhaps isHeadedBall fa = runIdentity $ do
			shortTermMemory ← return $ shortTermMemory { ballKickedOrHeaded = True}
		-- He was not allowed to get the ball
			return $ if not (wasAllowedToGetBall (ballIsFor longTermMemory) (nameOf team1) (nameOf team2) cn)
				then analyseActions team actions (punishMore RillegalBallPossession tef 1 shortTermMemory) longTermMemory
				else runIdentity $ do
					longTermMemory ← return $ longTermMemory { ballIsFor = Nothing}
					return $ if elem (playerID tef) (inOffsidePosition longTermMemory) && (offsidePossible longTermMemory) == OffsidePossible -- detect offside for wistle
						then analyseActions team actions (punishMore Roffside tef 0 shortTermMemory) { offsideAndTouchedBall=Just (playerID tef)} longTermMemory
						else runIdentity $ do
						-- A legal {Kick/Head}Ball may be illegal/dangerous play when ball was gained by keeper
							(team1Name,currHome) ← return $ if half == FirstHalf then (nameOf team1, West) else (nameOf team1,East)
							keeper1HadBall ← return $
								let keepers = filter isKeeper team1
								    keeper = head keepers
								in not (null keepers) && ballIsGainedBy (playerID keeper) ballState && inKeeperSpot keeper team1Name currHome field && (keeper1HadBall longTermMemory)
							keeper2HadBall ← return $
								let keepers = filter isKeeper team2
								    keeper = head keepers
								in not (null keepers) && ballIsGainedBy (playerID keeper) ballState && inKeeperSpot keeper team1Name currHome field && (keeper2HadBall longTermMemory)
							if keeper1HadBall && nmbr/=1 || keeper2HadBall && nmbr/=1
								then return $ analyseActions team actions (punishMore RdangerousPlay tef 5 shortTermMemory) longTermMemory
								else do
								-- A legal {Kick/Head}Ball may put people in a potential offside position
									longTermMemory ← return $ longTermMemory { lastKickedTheBall = Just (playerID tef)}
									(homeOfTeam1,homeOfTeam2) ← return $ if half == FirstHalf then (West,East) else (East,West)
									if team == Team1
										then do
											closestDefenderTeam2Xpos ← return $ offsideline field homeOfTeam2 theBall team2
											inOffsidePos ← return $ getOffsidePlayers field homeOfTeam1 closestDefenderTeam2Xpos (removeMember tef team1)
											return $ analyseActions team actions shortTermMemory longTermMemory { inOffsidePosition = inOffsidePos}
										else do
											closestDefenderTeam1Xpos ← return $ offsideline field homeOfTeam1 theBall team1
											inOffsidePos ← return $ getOffsidePlayers field homeOfTeam2 closestDefenderTeam1Xpos (removeMember tef team2)
											return $ analyseActions team actions shortTermMemory longTermMemory { inOffsidePosition = inOffsidePos}
		| perhaps isTackled fa = runIdentity $ do
		-- Store him, so we can look next round at the damage of the victim
			(victimID,ve) ← return $ case fromJust fa of (Tackled victimID ve _) → (victimID,ve)
			moreTackles ← return $ case filter (identifyPlayer victimID) (team1++team2) of
									(fb:_) → [(((playerID fb),(pos fb),(health fb)),((playerID tef),(pos tef),(health tef)),ve)]
									none → []
			shortTermMemory ← return $ shortTermMemory { thisRoundTackles = thisRoundTackles shortTermMemory ++ moreTackles}
			return $ analyseActions team actions shortTermMemory longTermMemory
		| perhaps isSchwalbed fa = runIdentity $ do
			opponents ← return $ if team == Team1 then team2 else team1
			opponentNear ← return $ not (null [posi | Player {pos=posi} ← opponents, dist (pos fb) posi <= 5.0])
			ballNear ← return $ dist zero { pxy=pos fb} (ballPos theBall) <= 10.0
			shortTermMemory ← return $ if (inPenaltyArea field (opponentHome team half) (pos fb)) -- In penaltyarea?
									 then shortTermMemory { penalty = penalty shortTermMemory ++ [(team,Rschwalbe)]}
								 else (if (opponentNear && ballNear) -- Was an opponent near and was the ball near? Then punish else 4
									then (punishMore Rschwalbe tef 4 shortTermMemory)
								 else (if ballNear -- Was the ball near? Then punish else 2
									then (punishMore Rschwalbe tef 2 shortTermMemory)
								 else (if opponentNear -- Was an opponent near? Then punish else 3
									then (punishMore Rschwalbe tef 3 shortTermMemory)
									else (punishMore Rschwalbe tef 1 shortTermMemory)
								 )))
			return $ analyseActions team actions shortTermMemory longTermMemory
		| perhaps isCaughtBall fa = runIdentity $ do
			return $ if isKeeper tef && inKeeperSpot tef team1Name currHome field -- is he a keeper (todo: and located in his penaltyarea (for everything, gaining ball, etc.)
				then analyseActions team actions shortTermMemory longTermMemory--ignoreThisAction
				else runIdentity $ do
					shortTermMemory ← return $ if (inPenaltyArea field (opponentHome team half) (pxy (ballPos theBall)))
											 then (punishMore Rhands tef 2 shortTermMemory) { penalty = penalty shortTermMemory ++ [(team,Rhands)]}
											 else (punishMore Rhands tef 4 shortTermMemory)
					return $ analyseActions team actions shortTermMemory longTermMemory
	-- theater, but no tackle or schwalbe was seen, so theater will be punished lightly
		| perhaps isPlayedTheater fa = analyseActions team actions (punishMore Rtheater tef 1 shortTermMemory) longTermMemory
	-- unknown action
		| otherwise = analyseActions team actions shortTermMemory longTermMemory--ignoreThisAction
		where
		(team1Name,currHome) = if half==FirstHalf then (nameOf team1,West) else (nameOf team1,East)
		Just tef = find (identifyPlayer PlayerID {clubName=cn,playerNo=nmbr}) allPlayers
		taf = tef
		fb = taf

		ignoreThisAction = analyseActions team actions shortTermMemory longTermMemory

		wasAllowedToGetBall ∷ (Maybe ATeam) → ClubName → ClubName → ClubName → Bool
		wasAllowedToGetBall Nothing _ _ _ = True
		wasAllowedToGetBall (Just team) team1 team2 myTeam
			| team == Team1 = team1 == myTeam
			| otherwise = team2 == myTeam

	analysePeopleOnTheGround ∷ [PlayerID] → IvanovShortTermMemory → Team → Team → IvanovShortTermMemory
	analysePeopleOnTheGround receivedYellow shortMem team1 team2
		| (gameStoppedForTackle shortMem) = shortMem -- have I stopped the game already for a tackle
		| otherwise = analysePeopleOnTheGround' receivedYellow shortMem [fb | fb ← team1 ++ team2, lastEventIsOnTheGround (effect fb)]
		where
		lastEventIsOnTheGround ∷ (Maybe PlayerEffect) → Bool
		lastEventIsOnTheGround es = isJust es && isOnTheGround (fromJust es)

		analysePeopleOnTheGround' ∷ [PlayerID] → IvanovShortTermMemory → [Player] → IvanovShortTermMemory
		analysePeopleOnTheGround' _ shortMem [] = shortMem
		analysePeopleOnTheGround' receivedYellow shortMem (fb@Player {playerID=PlayerID {clubName=cn}}:tefls) = runIdentity $ do
		-- how many opponents could have tackled him
			suspects ← return $ [s | s ← team1 ++ team2, dist (pos fb) (pos s) <= maxTackleReach s]
			healthDrop ← return $ case lookup (playerID fb) (prevHealthPlayers longTermMemory) of
												Just prevHealth → (health fb) - prevHealth
												none → zero
			motive4Schwalbe ← return $ ballDirectionIsTowardsGoal && ballNear
			goodMotive4Schwalbe ← return $ motive4Schwalbe && ballWasInPenaltyAreaOpponent
			schwalbeThreshold ← return $ if motive4Schwalbe
												then if goodMotive4Schwalbe
													then if victimReprimandedBefore then (0.5,0.4)  else (0.3,0.25)
													else if victimReprimandedBefore then (0.2,0.17) else (0.1,0.08)
												else if victimReprimandedBefore then (0.1,0.08) else (-999.0,-999.0)
			schwalbeThreshold ← return $ if length suspects > 1
												then (fst schwalbeThreshold * 0.5, snd schwalbeThreshold * 0.5)
												else schwalbeThreshold
			if not (null suspects) -- tackle or schwalbe??
				then do
					primarySuspect ← return $ suspects !! ((length suspects * round ((myMood shortMem) * 10.0)) `mod` (length suspects) )
					isNoOtherDefenderLeft ← return $ if (cn == nameOf team1 && half==FirstHalf || cn == nameOf team2 && half==SecondHalf)
													 then (null [fb | fb ← filter isFielder team2, (px (pos fb)) > (px (pos primarySuspect))])
													 else if (cn == nameOf team1 && half==SecondHalf || cn == nameOf team2 && half==FirstHalf)
														 then (null [fb | fb ← filter isFielder team2, (px (pos fb)) < px (pos primarySuspect)])
														 else (error "primary suspect of possible tackle does not play in one of the teams")
					opponentReprimandedBefore ← return $ elem (playerID primarySuspect) receivedYellow -- not (null (reprimands (events primarySuspect))) || (gotYellow (events primarySuspect))
					motive4Tackle ← return $ ballDirectionIsTowardsGoal && ballNear || ballGoingTowardsVictim
					goodMotive4Tackle ← return $ motive4Tackle && (isNoOtherDefenderLeft || goalVeryNear)
					tackleThreshold ← return $ if motive4Tackle
													then if goodMotive4Tackle
														then if opponentReprimandedBefore then (0.5,0.6)  else (0.7,0.75)
														else if opponentReprimandedBefore then (0.8,0.83) else (0.9,0.92)
													else if opponentReprimandedBefore then (0.9,0.92) else (999.0,999.0)
					shortMem ← return $ if ((myMood shortMem) >= fst tackleThreshold && (myMood shortMem) < snd tackleThreshold && healthDrop > 0.1) -- Give yellow for tackle
													 then (punishMore Rtackle primarySuspect 3 shortMem)
												 else (if ((myMood shortMem) >= fst tackleThreshold) -- Warn for tackle
													 then (punishMore Rtheater primarySuspect 1 shortMem)
												 else (if ((myMood shortMem) <= fst schwalbeThreshold && (myMood shortMem) > snd schwalbeThreshold && healthDrop > 0.1)-- then Give else yellow for schwalbe
													 then (punishMore Rschwalbe fb 3 shortMem)
												 else (if ((myMood shortMem) <= fst schwalbeThreshold) -- Warn for schwalbe
													 then (punishMore Rschwalbe fb 1 shortMem)
													 else shortMem
												 )))
					return $ analysePeopleOnTheGround' receivedYellow shortMem tefls
				else do
					shortMem ← return $ if ((myMood shortMem) <= fst schwalbeThreshold && (myMood shortMem) > snd schwalbeThreshold && healthDrop > 0.1) -- Warn good for schwalbe
														 then (punishMore Rschwalbe fb 2 shortMem)
													 else (if ((myMood shortMem) <= fst schwalbeThreshold) -- Warn for schwalbe
														 then (punishMore Rschwalbe fb 1 shortMem)
														 else shortMem
													 )
					return $ analysePeopleOnTheGround' receivedYellow shortMem tefls
			where
			goalVeryNear = if (cn == nameOf team1 && half == FirstHalf || cn == nameOf team2 && half == SecondHalf)
												then (dist (pos fb) Position {px=flength field,py=fwidth field/2.0} <= 20.0) -- east goal
											 else (if (cn == nameOf team1 && half == SecondHalf || cn == nameOf team2 && half == FirstHalf)
												then (dist (pos fb) Position {px=zero,py=fwidth field/2.0} <= 20.0) -- west goal
												else (error "fallen player is not from one of the teams"))
			ballNear = dist zero { pxy=pos fb} (ballPos theBall) <= 8.0
			victimReprimandedBefore = elem (playerID fb) receivedYellow
			ballWasInPenaltyAreaOpponent = cn == nameOf team1 && inPenaltyArea field (opponentHome Team1 half) (pxy (ballPos theBall))
												||
											 cn == nameOf team2 && inPenaltyArea field (opponentHome Team2 half) (pxy (ballPos theBall))
			ballWasInPenaltyAreaSelf = cn == nameOf team1 && inPenaltyArea field (opponentHome Team2 half) (pxy (ballPos theBall))
												||
											 cn == nameOf team2 && inPenaltyArea field (opponentHome Team1 half) (pxy (ballPos theBall))
			ballDirectionIsTowardsGoal = if (cn == nameOf team1 && half == FirstHalf || cn == nameOf team2 && half == SecondHalf)
												then ((direction (vxy (ballSpeed theBall))) > 0.5*pi && (direction (vxy (ballSpeed theBall))) < 1.5*pi) -- to east ..
											 else (if (cn == nameOf team1 && half == SecondHalf || cn == nameOf team2 && half == FirstHalf)
												then ((direction (vxy (ballSpeed theBall))) < 0.5*pi || (direction (vxy (ballSpeed theBall))) > 1.5*pi) -- to west .. PA: odd, why use ..||.. here and ..&&.. immediately above?
												else (error "fallen player is not from one of the teams"))
			ballGoingTowardsVictim = nextBallPos (ballPos theBall) (ballSpeed theBall) ((pos fb),5.0,(height fb))

	drawConclusions ∷ IvanovShortTermMemory → IvanovLongTermMemory → ([RefereeAction],IvanovLongTermMemory)
	drawConclusions shortMem longMem = runIdentity $ do
		longMem ← return $ longMem { keeper1HadBall = ballIsGainedBy PlayerID {playerNo=1,clubName=nameOf team1} ballState--all hasBall (filter isKeeper team1)
													 , keeper2HadBall = ballIsGainedBy PlayerID {playerNo=1,clubName=nameOf team2} ballState--all hasBall (filter isKeeper team2)
													 , prevHealthPlayers = map (\fb → ((playerID fb), (health fb))) (team1 ++ team2)
											 }
		teamPenalty ← return $ firstPenalty (penalty shortMem)
		offencePenalty ← return $ getMostImportantKickAction basicActions
		(kickAction,longMem) ← return $ if isJust teamPenalty
			then let t = fromJust teamPenalty in (Just (Penalty t), longMem { ballIsFor = Just t, typeOfKickoff = Just (Penalty t)})
			else if isJust offencePenalty
				then let p = fromJust offencePenalty in (offencePenalty, longMem { ballIsFor = Just (actionForWhichTeam p), typeOfKickoff = offencePenalty})
				else (Nothing,longMem)
		noticedActions ← return $ map fst basicActions
		longMem ← return $ if isJust kickAction then longMem { offsidePossible = KickForcedByReferee}
											 else (if ballKickedOrHeaded shortMem then (lowerOffsideCounter longMem)
																		 else longMem
											 )
		if not (gameStoppedForTackle shortMem) -- We haven't analysed tackles this round, it was a normal round
			then do
				let refActions = if isJust (ballIsFor longMem)
					then case typeOfKickoff longMem of
						Just kick → noticedActions ++ [DisplacePlayers (replacePlayers team1 team2 kick field theBall longMem)]
						Nothing → noticedActions
					else noticedActions
				let ng
					| time <= fromJust (gameLength longMem)/2.0 && half == FirstHalf = runIdentity $ do-- is it time to change half?
						longMem ← return $ longMem { waitingForSideSkipping = (True,150)}
						centerKick ← return $ CenterKick Team2
						let ds = kickoffDisplacements longMem
						return (refActions ++ [ContinueGame] ++ [EndHalf,centerKick,DisplacePlayers ds],longMem { ballIsFor = Just Team2, typeOfKickoff = Just centerKick})

					| time <= zero = (refActions ++ [GameOver],longMem) -- is it time to stop the game?
				-- Is the ball out of the lines and do we have: a goal, a corner, a goal kick, or a throw in
					| otherwise = runIdentity $ do
						(behindLineActions,longMem) ← return $ getBehindLinesActions half theBall field longMem
					-- No matter what happened, ball behind the line is ball behind the line
						if length behindLineActions > zero
							then do
								displacements ← return $ replacePlayers team1 team2 (last behindLineActions) field theBall longMem
								refActions ← return $ refActions ++ [DisplacePlayers displacements]
								return (refActions ++ [ContinueGame] ++ behindLineActions,longMem { offsidePossible = KickForcedByReferee})
							else do
								longMem ← return $ longMem { lastRoundTackles = thisRoundTackles shortMem}
							-- We have no new tackles for the next round
								let ng
									| null (thisRoundTackles shortMem) = runIdentity $ do
										let ng
											| isJust kickAction = runIdentity $ do
												kick ← return $ fromJust kickAction
												goalArea ← return $ kickActionIsFreeKickInPenaltyArea kick
												if isJust goalArea
													then do
														displacements ← return $ replacePlayers team1 team2 (fromJust goalArea) field theBall longMem
														return (refActions ++ [ContinueGame,DisplacePlayers displacements,fromJust goalArea],longMem)
													else return (refActions ++ [ContinueGame,kick],longMem)
											| otherwise = (refActions ++ [ContinueGame],longMem)
										return ng
								-- We have new tackles for the next round
									| otherwise = (refActions ++ [PauseGame,AddTime (1.0/30.0)],longMem)
								return ng
				return ng
	-- We have analysed tackles this round
			else
			-- We have a position to restart the game from
				if isJust kickAction
					then do
						kick ← return $ fromJust kickAction
						goalArea ← return $ kickActionIsFreeKickInPenaltyArea kick
						if isJust goalArea
							then do
								displacements ← return $ replacePlayers team1 team2 (fromJust goalArea) field theBall longMem
								return ([ContinueGame] ++ noticedActions ++ [DisplacePlayers displacements,fromJust goalArea],longMem)
							else do
								displacements ← return $ replacePlayers team1 team2 kick field theBall longMem
								return ([ContinueGame] ++ noticedActions ++ [DisplacePlayers displacements,kick],longMem)
			-- I still have to think about a new place to restart the game from
					else error "should have a position to restart the game from"
		where
		basicActions = getAllBasicActions (receivedYellow longMem) shortMem

		mirrorTeams ∷ Displacements → Displacements
		mirrorTeams ds = map (\(fbID,pos) → (fbID,mirror field pos)) ds

		kickActionIsFreeKickInPenaltyArea ∷ RefereeAction → Maybe RefereeAction
		kickActionIsFreeKickInPenaltyArea (DirectFreeKick team pos)
			| inPenaltyArea field (opponentHome team half) pos = Just (Penalty team)
			| inPenaltyArea field (teamHome team half) pos = Just (GoalKick team)
			| otherwise = Nothing
		kickActionIsFreeKickInPenaltyArea _ = Nothing

		getBehindLinesActions ∷ Half → Ball → Field → IvanovLongTermMemory → ([RefereeAction],IvanovLongTermMemory)
		getBehindLinesActions half theBall field longMem@IvanovLongTermMemory {lastKickedTheBall}
			| (pxy (ballPos theBall)) == p = ([],longMem) -- ball is not behind (see definition of pointToRectangle)
			| isNothing lastKickedTheBall = ([CenterKick Team1, DisplacePlayers $ kickoffDisplacements longMem],longMem {ballIsFor = Just Team1, typeOfKickoff = Just $ CenterKick Team1}) -- the ball got behind by unknown forces
			| (px (pxy (ballPos theBall))) < zero = runIdentity $ do -- behind line West?
				(team,homeTeamWest) ← return $ if half == FirstHalf then (Team1, nameOf team1) else (Team2,nameOf team2) -- first half?
				let ng
					| isbetween (py (pxy (ballPos theBall))) ((fwidth field)/2.0 - goalWidth/2.0) ((fwidth field)/2.0 + goalWidth/2.0) -- goal?
						&&
					 (pz (ballPos theBall)) < goalHeight = if (team==Team1)
					 	then ([Goal Team2,CenterKick Team1, DisplacePlayers $ kickoffDisplacements longMem],longMem { ballIsFor=Just Team1, typeOfKickoff = Just $ CenterKick Team1})
						else ([Goal Team1,CenterKick Team2, DisplacePlayers $ kickoffDisplacements longMem],longMem { ballIsFor=Just Team2, typeOfKickoff = Just $ CenterKick Team2})
					| clubName (fromJust lastKickedTheBall) == homeTeamWest = runIdentity $ do -- lastPlayerKicked Team1?
						corner ← return $ if (py (pxy (ballPos theBall))) > (fwidth field)/2.0 then South else North
						return ([Corner (other team) corner],longMem { ballIsFor = Just (other team), typeOfKickoff = Just $ Corner (other team) corner})
					| otherwise = ([GoalKick team],longMem { ballIsFor = Just team, typeOfKickoff = Just (GoalKick team)}) -- opponent team → goalkick
				return ng
			| (px (pxy (ballPos theBall))) > (flength field)= runIdentity $ do -- behind line East?
				(team,homeTeamEast) ← return $ if half == FirstHalf then (Team2, nameOf team2) else (Team1,nameOf team1)
				let ng
					| isbetween (py (pxy (ballPos theBall))) ((fwidth field)/2.0 - goalWidth/2.0) ((fwidth field)/2.0 + goalWidth/2.0) -- goal?
						&&
					 (pz (ballPos theBall)) < goalHeight = if (team==Team1)
					 	then ([Goal Team2,CenterKick Team1, DisplacePlayers $ kickoffDisplacements longMem],longMem { ballIsFor=Just Team1, typeOfKickoff = Just $ CenterKick Team1})
						else ([Goal Team1,CenterKick Team2, DisplacePlayers $ kickoffDisplacements longMem],longMem { ballIsFor=Just Team2, typeOfKickoff = Just $ CenterKick Team2})
					| clubName (fromJust lastKickedTheBall) == homeTeamEast = runIdentity $ do -- lastPlayerKicked Team1?
						corner ← return $ if (py (pxy (ballPos theBall))) > (fwidth field)/2.0 then South else North
						return ([Corner (other team) corner],longMem { ballIsFor = Just (other team), typeOfKickoff = Just $ Corner (other team) corner})
					| otherwise = ([GoalKick team],longMem { ballIsFor = Just team, typeOfKickoff = Just $ GoalKick team})
				return ng
			| otherwise = runIdentity $ do
				throwin ← return $ if clubName (fromJust lastKickedTheBall) == nameOf team1 then Team2 else Team1
				return ([ThrowIn throwin p],longMem { ballIsFor = Just throwin, typeOfKickoff = Just $ ThrowIn throwin p})
			where p = pointToRectangle (zero,Position {px=flength field,py=fwidth field}) (pxy (ballPos theBall))

		{- Most important (in given order): Tackle, OwnBallIllegally, Offside, DangerousPlay, Hands. Even: the rest -}
		getMostImportantKickAction ∷ [(RefereeAction,RefereeAction)] → Maybe RefereeAction
		getMostImportantKickAction actions = if null actions then Nothing else Just resumeAction where
			(_,resumeAction) = minimumBy (\(a1,r1) (a2,r2) → priority a1 `compare` priority a2) actions
			priority action
				| isTackleDetected action = 5
				| isOwnBallIllegally action = 4
				| isOffside action = 3
				| isDangerousPlay action = 2
				| isHands action = 1
				| otherwise = 0

		actionForWhichTeam ∷ RefereeAction → ATeam
		actionForWhichTeam (DirectFreeKick t _) = t
		actionForWhichTeam (GoalKick t) = t
		actionForWhichTeam (Corner t _) = t
		actionForWhichTeam (ThrowIn t _) = t
		actionForWhichTeam (Penalty t) = t
		actionForWhichTeam (CenterKick t) = t
		actionForWhichTeam _ = error "actionForWhichTeam: kick action expected"

		getAllBasicActions ∷ [PlayerID] → IvanovShortTermMemory → [(RefereeAction,RefereeAction)]
		getAllBasicActions receivedYellow shortMem = getAllBasicPunishActions receivedYellow (punished shortMem) where
		-- PA: this should really be programmed as a map→
			getAllBasicPunishActions ∷ [PlayerID] → [PunishedPlayer] → [(RefereeAction,RefereeAction)]
			getAllBasicPunishActions _ [] = []
			getAllBasicPunishActions receivedYellow ((fbID@PlayerID{clubName=cn},punish,[]):pals)
				| punish > 4 = ((ReprimandPlayer fbID RedCard,DirectFreeKick otherTeam kickPos) : rest)
				| punish >= 3 = runIdentity $ do
					yellow ← return $ (ReprimandPlayer fbID YellowCard,DirectFreeKick otherTeam kickPos)
					red ← return $ (ReprimandPlayer fbID RedCard, DirectFreeKick otherTeam kickPos)
					let ng
						| elem fbID receivedYellow = yellow : red : rest
						| otherwise = yellow : rest
					return ng
				| punish >= 1 = (ReprimandPlayer fbID Warning,DirectFreeKick otherTeam kickPos) : rest
				| otherwise = rest
				where
				kickPosHome = if cn == nameOf team1 && half == FirstHalf || cn == nameOf team2 && half == SecondHalf then East else West
				kickPosTeam = if cn == nameOf team1 then team2 else team1
				kickPos = posForFreeKick kickPosHome (pxy (ballPos theBall)) kickPosTeam
				otherTeam = if cn == nameOf team1 then Team2 else Team1
				rest = getAllBasicPunishActions receivedYellow pals
			getAllBasicPunishActions receivedYellow ((fbID@PlayerID{clubName=cn},punish,(r:rls)):pals)
				| r == Rtackle = (TackleDetected fbID,DirectFreeKick otherTeam kickPos) : rest
				| r == Roffside && isJust (offsideAndTouchedBall shortMem) =
					let ng
						| fromJust (offsideAndTouchedBall shortMem) == fbID = (Offside fbID,DirectFreeKick otherTeam kickPos) : rest
						| otherwise = rest
					in ng
				| r == Rtheater = (TheaterDetected fbID,DirectFreeKick otherTeam kickPos) : rest
				| r == Rschwalbe = (SchwalbeDetected fbID,DirectFreeKick otherTeam kickPos) : rest
				| r == Rhands = (Hands fbID,DirectFreeKick otherTeam kickPos) : rest
				| r == RdangerousPlay = (DangerousPlay fbID,DirectFreeKick otherTeam kickPos) : rest
				| r == RillegalBallPossession = (OwnBallIllegally fbID,DirectFreeKick otherTeam kickPos) : rest
				| otherwise = error "getAllBasicPunishActions: unknown reason of RefereeAction."
				where
				kickPosHome = if cn == nameOf team1 && half == FirstHalf || cn == nameOf team2 && half == SecondHalf then East else West
				kickPosTeam = if cn == nameOf team1 then team2 else team1
				kickPos = posForFreeKick kickPosHome (pxy (ballPos theBall)) kickPosTeam
				otherTeam = if cn == nameOf team1 then Team2 else Team1
				rest = getAllBasicPunishActions receivedYellow ((fbID,punish,rls):pals)

		firstPenalty ∷ [(ATeam,Reason)] → Maybe ATeam
		firstPenalty [] = Nothing
		firstPenalty ((t,_):_) = Just (other t)

isTackleVictim ∷ PlayerID → [TackleAction] → Bool
isTackleVictim fbID tackleEvents = any (\((fbID',_,_),_,_) → fbID == fbID') tackleEvents

isTackleOffender ∷ PlayerID → [TackleAction] → Bool
isTackleOffender fbID tackleEvents = any (\(_,(fbID',_,_),_) → fbID == fbID') tackleEvents

nearLastRoundTackle ∷ Position → [TackleAction] → Bool
nearLastRoundTackle pos tackleEvents = all near tackleEvents where
	near ((_,vpos,_),(_,opos,_),_) = dist pos vpos > nearEventRadius && dist pos opos > nearEventRadius

-- Used for offside, so we only look at the xposition
offsideline ∷ Field → Home → Ball → [Player] → Metre
offsideline field home ball fbs = case sortBy cmp ((px (pxy (ballPos ball))) : [(px (pos fb)) | fb ← fbs]) of
													[] → edge
													[x] → x
													[_,x] → x
													(_:_:x:_) → x
	where
	(cmp,edge) = if (home == West)
		then (compare,zero)
		else (\x y → compare y x,(flength field))

getOffsidePlayers ∷ Field → Home → XPos → [Player] → [PlayerID]
getOffsidePlayers field home line fbs = map playerIdentity (filter (\fb@Player{pos=Position{px=posx}} -> atOtherHalf posx && (cmp (px (pos fb)) line)) fbs) where
	(cmp,atOtherHalf) = if home == West then ((>),(<) fieldhalf) else ((<),(>) fieldhalf)
	fieldhalf = flength field / 2.0

-- A referee can not see everything correct.
filterOutMeanActions ∷ [Player] → [PlayerWithEffect] → StdGen → ([PlayerWithEffect],StdGen)
filterOutMeanActions allPlayers allActions seed = ([action | (p,action) ← zip ps allActions, noticeAction action p],seed1) where
	(ps,seed1) = iterateStn (length allActions) random seed

	noticeAction ∷ PlayerWithEffect → Float → Bool
	noticeAction (fa,playerID) p
		| foul = avg [p,1.0 - pa + if elem skill (skillsAsList fb) then 0.1 else 0.0] <= 0.5
		| otherwise = True
		where
		Just fb = find (identifyPlayer playerID) allPlayers
		(foul,pa,skill) = if perhaps isTackled fa then (True,chanceOfPenaltySuccess, Tackling)
							 else (if perhaps isSchwalbed fa then (True,chanceOfSchwalbeSuccess,Schwalbing)
							 else (if perhaps isPlayedTheater fa then (True,chanceOfTheaterSuccess, PlayingTheater)
							 else (if perhaps isCaughtBall fa then (True,chanceOfCatchSuccess, Catching)
													 else (False,error "ASNTD",error "ISAONE")
							 )))

nextBallPos ∷ Position3D → Speed3D → (Position,XRadius,ZRadius) → Bool
nextBallPos item dir (target,radius,height) = isGettingToPos (iterate (nextPos dir) item) where
	targetAtGround = zero { pxy=target}
	targetInAir = Position3D {pxy=target, pz=height}

	nextPos ∷ Speed3D → Position3D → Position3D
	nextPos speed currentPosition = movePoint3D RVector3D {dxy=RVector{dx=cos (direction (vxy speed))*newV,dy=sin (direction (vxy speed))*newV},dz=newV3} currentPosition
		where
		resistance = if (pz currentPosition) > zero then airResistance else surfaceResistance
		newV = resistance * velocity (vxy speed)
		newV3 = vz speed - 0.1*accellerationSec

	isGettingToPos ∷ [Position3D] → Bool
	isGettingToPos [] = False
	isGettingToPos [x] = False
	isGettingToPos (x:y:xs)
		| dist x targetAtGround < radius || dist x targetInAir < radius = True
		| avg [dist x targetAtGround,dist x targetInAir] < avg [dist y targetAtGround,dist y targetInAir] = False -- object is moving away
		| otherwise = isGettingToPos (y:xs) -- object is coming closer


-- Freekicks are granted from the pos of the player that is the most closest to the ball but not a position more forward to the goal
posForFreeKick ∷ Home → Position {-→ Position-} → [Player] → Position
posForFreeKick home ballPos {-victimPos-} [] = ballPos--victimPos
posForFreeKick home ballPos {-victimPos-} team = case [fb | fb ← team, (dist (pos fb) ballPos) <= 15.0 ] of
		[] → ballPos
		close → pos (foldr1 closer2ball close)
	where
	closer2ball ∷ Player → Player → Player
	closer2ball fb1 fb2
		| dist (pos fb1) ballPos < dist (pos fb2) ballPos = fb1
		| otherwise = fb2

inKeeperSpot ∷ Player → ClubName → Home → Field → Bool
inKeeperSpot fb clubName home field = inPenaltyArea field (if getClubName fb == clubName then home else (other home)) (pos fb)
