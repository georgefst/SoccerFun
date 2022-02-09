{-# LANGUAGE UnicodeSyntax, TemplateHaskell #-}
module SoccerFun.MatchControl (Match (..), Score, NrOfGoals, setMatchStart, Step, stepMatch, lookupPlayer) where

import Prelude.Unicode
import SoccerFun.Prelude
import SoccerFun.Geometry
import System.Random
import SoccerFun.Types
import SoccerFun.Referee
import SoccerFun.Random
import SoccerFun.Team
import SoccerFun.Ball
import SoccerFun.Player
import Control.Monad.Identity
import Control.Monad.State (State, runState)
import Data.Maybe
import Data.List
import SoccerFun.Field

data Match = Match
	{team1       ∷ Team,         -- ^ team1
	 team2       ∷ Team,         -- ^ team2
	 theBall     ∷ BallState,    -- ^ the whereabouts of the ball
	 theField    ∷ Field,        -- ^ the ball field
	 theReferee  ∷ Referee,      -- ^ the referee
	 playingHalf ∷ Half,         -- ^ first half or second half team1 plays West at first half and East at second half
	 playingTime ∷ PlayingTime,  -- ^ todo: add a boolean gameOver, playingtime will not walk back to (zero) and its up to the referee at which time he is to end the game
	 score       ∷ Score,        -- ^ the score
	 seed        ∷ StdGen,       -- ^ random seed for generating pseudo random values
	 unittime    ∷ TimeUnit     -- ^ the time unit of a single simulation step
	} deriving Show

type Score			= (NrOfGoals,NrOfGoals)			-- ^ (goals by Team1, goals by Team2)
type NrOfGoals		= Int								-- ^ (zero) <= nr of goals

lookupPlayer ∷ PlayerID → Match → Maybe Player
lookupPlayer pid Match {team1 = t1, team2 = t2} = find ((==) pid ∘ playerID) (t1 ⧺ t2)

setMatchStart ∷ Team → Team → Field → Referee → PlayingTime → StdGen → Match
setMatchStart fstTeam sndTeam field referee time rs = Match
	{team1       = validateTeam fstTeam,
	 team2       = validateTeam sndTeam,
	 theBall     = Free (ballAtCenter field),
	 theField    = field,
	 theReferee  = referee,
	 playingHalf = FirstHalf,
	 playingTime = time,
	 unittime    = 0.05,
	 score       = (0,0),
	 seed        = rs}

type Step = (([RefereeAction],[PlayerWithAction]),Match)

stepMatch ∷ Match → Step
stepMatch match = runIdentity $ do
	(refereeActions,  match)		← return $ refereeTurn match
	match							← return $ performRefereeActions refereeActions match
	(intendedActions, match)		← return $ playersTurn   refereeActions  match
	(succeededActions,match)		← return $ selectActions intendedActions match
	match							← return $ performPlayerActions intendedActions succeededActions match
	match							← return $ advanceTime match
	return $ ((refereeActions,succeededActions),match)

-- | lets every player player conjure an initiative
playersTurn ∷ [RefereeAction] → Match → ([PlayerWithAction],Match)
playersTurn refereeActions match = (intendedActions, match {team1 = map snd actionsOfTeam1,team2 = map snd actionsOfTeam2}) where
	actionsOfTeam1				= map (think refereeActions (theBall match) (team2 match)) (singleOutElems (team1 match))
	actionsOfTeam2				= map (think refereeActions (theBall match) (team1 match)) (singleOutElems (team2 match))
	intendedActions				= [(action,playerID) | (action,Player{playerID=playerID}) <- actionsOfTeam1 ++ actionsOfTeam2]

	think ∷ [RefereeAction] → BallState → [Player] → (Player,[Player]) → (PlayerAction,Player)
	think refereeActions ballstate opponents (me@Player{  effect=effect,brain=brain@Brain{ai=ai,m=m}},ownTeam)
		| isNothing effect		= (action,newMe)
		| otherwise				= checkIfNotOnGround (fromJust effect) action newMe
		where
		(action,newM)			= runState (ai $ BrainInput{referee=refereeActions,ball=ballstate,others=ownTeam ++ opponents,me=me}) m
		newMe = clonePlayer (brain{ai=ai,m=newM}) me {  effect=effect}

		checkIfNotOnGround ∷ PlayerEffect → PlayerAction → Player → (PlayerAction,Player)
		checkIfNotOnGround (OnTheGround i) action fb
			| i <= 0			= (action,              fb { effect = Nothing})
			| otherwise			= (allowOnlyPlayTheater,fb { effect = Just (OnTheGround (i-1))
									                         , stamina= alterStamina ballstate fb (zero) (zero)})
			where
			allowOnlyPlayTheater= case action of
									PlayTheater	→ action
									_			→ Move (zero) { direction = (direction (speed fb))} (zero) -- Run (zero) { direction = (direction (speed fb))}
		checkIfNotOnGround _ action fb
								= (action,fb)

{-	selectActions actions match
		removes all failing actions, and returns the list of remaining succeeding actions.
		It updates the random stream in match, and neutralizes actions of tackled players.
-}
selectActions ∷ [PlayerWithAction] → Match → ([PlayerWithAction],Match)
selectActions actions match@Match{seed=seed} = runIdentity $ do
	(seed,succeededActions)				← return $ validActions match seed actions
	((successTackles,failedTackles),seed)	← return $ analyseTackleActions match succeededActions seed
	succeededActions						← return $ filter (isNoTackleVictim successTackles) (removeMembers succeededActions failedTackles)
	return $ (succeededActions,match { seed=seed})
	where
	{-	actions that are always valid: {Move, Run, Rotate, Feint, Schwalbe, PlayTheater}
		actions that may have success: {Tackle} (looked at later at performactions)
		actions where at most (fromIntegral 1) can succeed: {GainBall, KickBall, HeadBall, Catch}
	-}
	validActions ∷ Match → StdGen → [PlayerWithAction] → (StdGen,[PlayerWithAction])
	validActions match seed []			= (seed,[])
	validActions match seed actions
		| isJust ballAction			= (seed1,(fromJust ballAction:otherActions))
		| otherwise						= (seed1,otherActions)
		where
		allPlayers					= (team1 match) ++ (team2 match)
		(ballActions,otherActions)	= spanfilter (isActionOnBall ∘ fst) actions
		(seed1,ballAction)				= selectBallAction (theBall match) allPlayers seed ballActions

		selectBallAction ∷ BallState → [Player] → StdGen → [PlayerWithAction] → (StdGen,Maybe PlayerWithAction)
		selectBallAction ballstate allPlayers seed desiredActions = runIdentity $ do
			(ps,seed)					← return $ iterateStn (length desiredActions) random seed
			odds						← return $ [ (successOfAction ballstate allPlayers action (if (p==fromIntegral 1) then p else (makeRandomRealistic p)),action)
										  | (action,p) <- zip desiredActions ps
										  ]
			okOdds					← return $ filter (\(p,a) → p > (zero)) odds
			if null okOdds			then return $ (seed,Nothing) else do
				maxOddProb				← return $ maximum (map fst okOdds)
				bestActions				← return $ [a | (p,a) <- okOdds, p >= maxOddProb]
				(p,seed)					← return $ random seed
				if p==fromIntegral 1					then return $ (seed,Just (head bestActions)) else do
					let l = fromIntegral $ length bestActions ∷ Float
					let m = p * l
					let idx = floor m
					return (seed,Just (bestActions !! idx))
			where
			successOfAction ∷ BallState → [Player] → PlayerWithAction → Float → Float
			successOfAction ballstate allPlayers (action,who) p
										= myFatigue * myHealth * p * successOfAction
				where
				successOfAction		= if (isGainBall  action && ballGainable  && ballAtGainSpeed) then  successGaining else
										 (if (isCatchBall action && ballCatchable && ballAtCatchSpeed) then successCatching else
										 (if (isKickBall  action && ballKickable) then                      successKicking else
										 (if (isHeadBall  action && ballHeadable) then                      successHeading else
											 	                                                        (zero)
										 )))
				Just me						= find (identifyPlayer who) allPlayers
				myFatigue 				= (stamina me)
				myHealth				= (health me)
				mySkills 				= skillsAsList me
				myLength 				= (height me)
				iGainWell				= elem Gaining  mySkills
				iKickWell				= elem Kicking  mySkills
				iHeadWell				= elem Heading  mySkills
				iCatchWell				= elem Catching mySkills

				ballGainable			= dPlayerBall <= maxGainReach  me
				ballKickable			= dPlayerBall <= maxKickReach  me
				ballHeadable			= dPlayerBall <= maxHeadReach  me
				ballCatchable			= dPlayerBall <= maxCatchReach me
				ballAtGainSpeed			= dVelocity    <= maxGainVelocityDifference  me dPlayerBall
				ballAtCatchSpeed		= dVelocity    <= maxCatchVelocityDifference me dPlayerBall
				dSpeed 				= (zero) { dxy = scaleVector (velocity (speed me)) (toRVector (direction (speed me)))}
											-
										  RVector3D {       dxy = scaleVector (velocity (vxy (ballSpeed theBall))) (toRVector (direction (vxy (ballSpeed theBall))))
										  ,       dz  = (vz (ballSpeed theBall))
										  }
				dVelocity 				= sizeVector3D dSpeed

				theBall 				= getBall ballstate allPlayers
				dPlayerBall 			= dist (toPosition3D (pos me)) (ballPos theBall)

				othersWithBall		= [fb | fb <- allPlayers, ballIsGainedBy (playerID fb) ballstate && not (identifyPlayer who fb)]
				otherHasBall			= not (null othersWithBall)
				otherDribblesWell		= elem Dribbling (skillsAsList (head othersWithBall))

				successGaining			= if (ballIsFree ballstate) then (lengthPenalty * if iGainWell then 0.95 else 0.8) else
										 (if  otherHasBall        then (lengthPenalty * if iGainWell then 0.75 else 0.3 * if otherDribblesWell then 0.6 else 1.0)
												                     else 1.0)
				successKicking			= if (ballIsFree ballstate) then (lengthBonus * if iKickWell then 0.95 else 0.85) else
										 (if  otherHasBall        then (lengthBonus * if iKickWell then 0.80 else 0.70 * if otherDribblesWell then 0.7 else 1.0)
												                     else 1.0)
				successHeading			= if iHeadWell then  0.95 else 0.9
				successCatching		= if iCatchWell then 1.0 else  0.95
				lengthBonus				= (myLength-1.2) ** 0.15
				lengthPenalty			= (2.6-myLength) ** 0.1

analyseTackleActions ∷ Match → [PlayerWithAction] → StdGen → (([PlayerWithAction],[PlayerWithAction]),StdGen)
analyseTackleActions match performedActions seed
	= spanfilterSt (isPossibleTackle match) [action | action <- performedActions, isPlayerTackle (fst action)] seed
	where
	isPossibleTackle ∷ Match → PlayerWithAction → StdGen → (Bool,StdGen)
	isPossibleTackle match@Match{team1=team1,team2=team2} (Tackle victimID _,playerID) seed
		| dMeVictim > maxTackleReach offender								--  victim is out of reach
						= (False,seed)
		| otherwise		= (((p + chanceOfSuccess) / 2) > 0.5,seed')				-- victim is within reach, but tackle may fail
		where
		(p,seed')		= random seed
		allPlayers		= team1 ++ team2
		Just offender		= find (identifyPlayer playerID) allPlayers
		Just victim			= find (identifyPlayer victimID) allPlayers
		dMeVictim		= dist (pos offender) (pos victim)
		chanceOfSuccess	= (1.0 - dMeVictim + if (elem Tackling (skillsAsList offender)) then 0.9 else 0.7) /2

isNoTackleVictim ∷ [PlayerWithAction] → PlayerWithAction → Bool
isNoTackleVictim tackles (action,playerID)
	= isSchwalbe action
	∨ isPlayTheater action
	∨ null [victim | (Tackle victim _,_) <- tackles, victim==playerID]

{-	refereeTurn match
		determines whether the rules of soccer are adhered to and yields a list of referee actions.
-}
refereeTurn ∷ Match → ([RefereeAction],Match)
refereeTurn match@Match{theReferee=referee@Referee{  rbrain=brain@Brain{ai=ai,m=m}},theBall=theBall,playingHalf=playingHalf,team1=team1,team2=team2,playingTime=playingTime,unittime=unittime,seed=seed}
										= (refereeActions,match { theReferee=newReferee,seed=newSeed})
	where
	(refereeActions,(newM,newSeed))	= ai playingTime unittime theBall playingHalf team1 team2 (m,seed)
	newReferee							= cloneReferee (Brain{ai=ai,m=newM}) referee

{-	performRefereeActions refereeActions match
		performs for each ball player in match his succeededAction, informs them about the referee actions, and moves the ball.
-}
performRefereeActions ∷ [RefereeAction] → Match → Match
performRefereeActions refActions match	= foldl doRefereeEvent match refActions
	where
	doRefereeEvent ∷ Match → RefereeAction → Match
	doRefereeEvent theMatch@Match{playingHalf=playingHalf,theField=theField,team1=team1,team2=team2} refereeAction
		| isAlterMatchBallAndTeams		= theMatch { theBall=Free (mkBall pos (zero))}
		| isGameProgressEvent			= gameProgress theMatch
		| isDisplaceTeamsEvent			= theMatch { team1=map (displacePlayer ds) team1,team2=map (displacePlayer ds) team2}
		| isReprimandEvent				= let (team1',team2')		= reprimandPlayer (nameOf team1) tef repr (team1,team2) in theMatch { team1=team1',team2=team2'}
		| otherwise						= theMatch
		where
		(isAlterMatchBallAndTeams,pos)	= case refereeAction of
											DirectFreeKick _ pos	→ (True,pos)
											ThrowIn        _ pos	→ (True,pos)
											Corner         _ _		→ (True,fromJust (getKickPos theField playingHalf refereeAction))
											GoalKick       _		→ (True,fromJust (getKickPos theField playingHalf refereeAction))
											Penalty        _		→ (True,fromJust (getKickPos theField playingHalf refereeAction))
											CenterKick     _		→ (True,fromJust (getKickPos theField playingHalf refereeAction))
											otherwise				→ (False,error "UNDEF pos")
		(isGameProgressEvent,gameProgress)
										= case refereeAction of
											GameOver				→ (True,\m                → m { playingTime=(zero)})
											AddTime t				→ (True,\m                → m { playingTime=(playingTime m+)t})
											EndHalf					→ (True,\m                →         m { playingHalf=SecondHalf})
											Goal    t				→ (True,\m@Match{score=(w,e)} →         m { score=if (t==Team1) then (w+1,e) else (w,e+1)})
											otherwise				→ (False,error "UNDEF gameProgress")
		(isDisplaceTeamsEvent,ds)		= case refereeAction of
											DisplacePlayers ds		→ (True, ds)
											otherwise				→ (False,error "UNDEF ds")
		(isReprimandEvent,(tef,repr))	= case refereeAction of
											ReprimandPlayer p r		→ (True, (p,r))
											otherwise				→ (False,(error "UNDEF tef", error "UNDEF repr"))

displacePlayer ∷ Displacements → Player → Player
displacePlayer displacements fb
	= case lookup (playerID fb) displacements of
		Just pos				→ fb { pos=pos}
		nothing					→ fb

reprimandPlayer ∷ ClubName → PlayerID → Reprimand → ([Player],[Player]) → ([Player],[Player])
reprimandPlayer club1 playerID RedCard (team1,team2)
	= splitAt (nrPlayers1 - if ((clubName playerID) == club1) then 1 else 0) (uneq1++uneq2)
	where
	(uneq1,_,uneq2)				= break1 (identifyPlayer playerID) (team1++team2)
	nrPlayers1				= length team1
reprimandPlayer _ _ _ teams		= teams

{-	performPlayerActions actions succeededActions match
		performs for each ball player in match his succeededAction and moves the ball.
-}
performPlayerActions ∷ [PlayerWithAction] → [PlayerWithAction] → Match → Match
performPlayerActions actions succeededActions match@Match{theField=theField,theBall=theBall,team1=team1,team2=team2,seed=seed,unittime=unittime} = runIdentity $ do
	(seed,ball,newPlayers1,newPlayers2)
					← return $ foldl (flip (performAction succeededActions)) (seed,theBall,team1,team2) actions
	(ball,seed)	← return $ moveBall theField (newPlayers1++newPlayers2) (ball,seed)
	match			←  return $ match { team1=newPlayers1, team2=newPlayers2, theBall=ball, seed=seed }
	return $ match
	where
	performAction ∷ [PlayerWithAction] → PlayerWithAction → (StdGen,BallState,[Player],[Player])
		                                                          → (StdGen,BallState,[Player],[Player])
	performAction succeededActions initiative (seed,ball,allPlayers1,allPlayers2)
		| elem initiative succeededActions				-- plan has succeeded
			= performAction' initiative (seed,ball,allPlayers1,allPlayers2)
		| otherwise											-- plan has failed
			= (seed,ball,map (failThisPlayerAction initiative) allPlayers1,map (failThisPlayerAction initiative) allPlayers2)
		where
		failThisPlayerAction ∷ PlayerWithAction → Player → Player
		failThisPlayerAction (idea,playerID) fb
			| identifyPlayer playerID fb
								= fb { effect=Just (failPlayerAction idea)}
			| otherwise			= fb

		performAction' ∷ PlayerWithAction → (StdGen,BallState,[Player],[Player])
							                   → (StdGen,BallState,[Player],[Player])

		performAction' (Move sp angle,playerID) (seed,ball,team1,team2) = runIdentity $ do
			(team1,team2)		← return $ splitAt (length team1) (unbreak1 (uneq1,newFb,uneq2))
			return (seed1,ball,team1,team2)
			where
			(uneq1,fb,uneq2)	= break1 (identifyPlayer playerID) (team1 ++ team2)
			curNose			= (nose fb)
			curSpeed			= (speed fb)
			skills				= skillsAsList fb
			playerHasBall		= ballIsGainedBy playerID ball
			(p,seed1)			= random seed
			feasibleAngle		= ((signum angle)) * (abs angle `boundedBy` (0, maxRotateAngle fb))
			newNose			= curNose + feasibleAngle
			angleDifficulty 	= angleHowFarFromPi ((direction sp)-newNose)
			angleDifference		= angleHowFarFromAngle (direction sp) newNose
			newStamina			= max (alterStamina ball fb   angleDifficulty angleDifference) maxStamina
			newHealth			= alterHealth  ball fb p angleDifficulty angleDifference
			healthFat			= getHealthStaminaFactor newHealth newStamina
			newVel				= healthFat * (velocity sp `boundedBy` (0, maxVelocity fb angleDifficulty angleDifference))
			newSpeed			= sp { velocity=newVel}
			newPosition'		= movePoint (scaleVector (unittime * newVel) (toRVector (direction newSpeed))) (pos fb)
			newPosition		= pointToRectangle ((zero),Position{px=(flength theField),py=(fwidth theField)}) newPosition'
			newFb				= fb { stamina = newStamina
									   , health  = newHealth
									   , speed   = newSpeed
									   , pos     = newPosition
									   , nose    = newNose
									   , effect  = Just (Moved newSpeed feasibleAngle)
								  }

		{-	Run has become deprecated.
			Rules for running:
			(1) you can't run through another player
			(2) you can't run faster than maximum velocity for a player (depends on Running and Dribbling skill)
			(3) you can't leave field
			(4) running fast lowers your stamina
			(5) running slow increases your stamina and health
			(6) poor health or poor stamina lowers your maximum velocity
		-}{-
		performAction' (Run speed,playerID) (seed,ball,team1,team2)
			| null bumpedInto	= runIdentity $ do -- no collision with other player
				newFb		← fb { pos     = newPosition
									   , speed   = newSpeed
									   , stamina = newStamina
									   , effect  = Just (Ran eventSpeed { direction = (direction eventSpeed) - (direction (speed fb))})
								  }
				(team1,team2)	← splitAt (length team1) (unbreak1 (uneq1,newFb,uneq2))
				return $ (seed2,ball,team1,team2)
			| otherwise		= runIdentity $ do		-- collission with other player
				playerSpeed	← (speed fb) { direction=(direction newSpeed), velocity=(velocity 0.3*newSpeed)}
				newFb		← fb { speed   = playerSpeed
									   , stamina = newStamina
									   , effect  = Just (Ran (speed fb) { velocity=(velocity 0.3*newSpeed)})
								  }
				(team1,team2)	← splitAt (length team1) (unbreak1 (uneq1,newFb,uneq2))
				(p3,seed3)	← random seed2
				(seed4,team1,team2)
								← moveBumpedPlayers newSpeed { velocity=(velocity 0.3*newSpeed), direction = (direction 0.4*p3*newSpeed)}
									bumpedInto (seed3,team1,team2)
				return $ (seed4,ball,team1,team2)
			where
			(uneq1,fb,uneq2)	= break1 (identifyPlayer playerID) (team1 ++ team2)
			(p1,seed1)			= random seed
			(p2,seed2)			= random seed1
			angleDifficulty 	= angleHowFarFromPi (direction speed)
			angleDifference		= if (isNothing (effect fb)) then (zero) else (case fromJust (effect fb) of
																	   Ran s	→ angleHowFarFromAngle (direction speed) (direction s)
																	   _		→ (zero))
			playerHasBall		= ballIsGainedBy playerID ball
			newStamina			= alterStamina ball fb    angleDifficulty angleDifference
			newHealth			= alterHealth  ball fb p1 angleDifficulty angleDifference
			healthFat			= getHealthStaminaFactor newHealth newStamina
			newVel				= healthFat * (velocity speed `boundedBy` (0, maxVelocity fb angleDifficulty angleDifference))
			newSpeed			= (speed fb) { velocity=newVel}
			newAngle			= if (p2 == (fromIntegral 1)) then ((direction (speed fb)) + (direction speed)) else
								 (if (p2 > 0.5) then  ((direction (speed fb)) + (0.85 + 0.15 * healthFat) * pi - pi + (direction speed)) else
									              ((direction (speed fb)) + (1.15 - 0.15 * healthFat) * pi - pi + (direction speed)))
			eventSpeed			= (speed fb) { velocity=newVel, direction=newAngle}
			newPosition'		= movePoint (scaleVector (unittime * newVel) (toRVector newAngle)) (pos fb)
			newPosition		= pointToRectangle ((zero),{px=(flength theField),py=(fwidth theField)}) newPosition'
			bumpedInto			= (playerID [fb) | fb <- uneq1++uneq2 | inRadiusOfPlayer newPosition fb]
		-}
		{- Rotate has become deprecated:
			Rules for rotating:
			(1) Rotating with slow velocity increases your stamina and health
			(2) Rotating with high velocity lowers your stamina and health
			(3) poor health or poor stamina lowers your maximum velocity
			(4) poor health or poor stamina lowers your precision with turning
			(5) you can't leave field
			(6) you can't run faster than your maximum velocity
			(7) you can't run through another player
		-}{-
		performAction' (Rotate speed,playerID) (seed,ball,team1,team2)
			| not (null bumpedInto) = runIdentity $ do
				playerSpeed			← (speed fb) { direction=newAngle, velocity=(velocity 0.3*newSpeed)}
				newFb				← fb { speed   = playerSpeed
											   , stamina = newStamina
											   , effect  = Just (Rotated {direction=newAngle,velocity=(velocity 0.3*newSpeed)})
										  }
				(team1,team2)			← splitAt (length team1) (unbreak1 (uneq1,newFb,uneq2))
				(p2,seed2)			← random seed1
				(seed3,team1,team2)	← moveBumpedPlayers newSpeed { velocity=(velocity 0.3*newSpeed), direction = (direction 0.4*p2*newSpeed)}
											bumpedInto (seed2,team1,team2)
				return $ (seed3,ball,team1,team2)
			| otherwise = runIdentity $ do
				newFb				← fb { pos     = newPosition
											   , speed   = newSpeed
											   , stamina = newStamina
											   , effect  = Just (Rotated newSpeed)
										  }
				(newTeam1,newTeam2)	← splitAt (length team1) (unbreak1 (uneq1,newFb,uneq2))
				return $ (seed1,ball,newTeam1,newTeam2)
			where
			(uneq1,fb,uneq2)	= break1 (identifyPlayer playerID) (team1 ++ team2)
			(p1,seed1)			= random seed
			newV'				= healthFat * (setbetween (velocity speed) (zero) (maxVelocity fb (zero) (zero)))
			newV				= if (newAngle >= maxRotateAngle fb { speed=(speed fb) { velocity=newV'}}) then (max (newV' - 0.5) (zero)) else newV'
			newAngle			= let lengthFactor	= if (elem Rotating skills) then ((height fb)-0.2) else (height fb)
									   healthFactor	= (healthFat/((direction 0.75*lengthFactor))*speed)
									in if ((direction speed) > (zero)) then (min (  maxRotateAngle fb)  healthFactor) else
									                               (max (~(maxRotateAngle fb)) healthFactor)
			skills				= skillsAsList fb
			playerHasBall		= ballIsGainedBy playerID ball
			newStamina			= alterStamina ball fb    (zero) (zero)
			newHealth			= alterHealth  ball fb p1 (zero) (zero)
			healthFat			= getHealthStaminaFactor newHealth newStamina
			newDirection		= (direction (speed fb)) + newAngle
			newSpeed			= (speed fb) { direction=newDirection, velocity=newV}
			newPosition'		= movePoint (scaleVector (unittime * newV) (toRVector newDirection)) (pos fb)
			newPosition		= pointToRectangle ((zero),{px=(flength theField),py=(fwidth theField)}) newPosition'
			bumpedInto			= (playerID [fb) | fb <- uneq1++uneq2 | inRadiusOfPlayer newPosition fb]
		-}
		{-	Rules for gaining ball:
			(1) ball obtains position and surface speed of obtaining player
		-}
		performAction' (GainBall,playerID) (seed,ball,team1,team2) = runIdentity $ do
			(team1,team2)		← return $ splitAt (length team1) (unbreak1 (uneq1,newFb,uneq2))
			return $ (seed,GainedBy playerID,team1,team2)
			where
			(uneq1,fb,uneq2)	= break1 (identifyPlayer playerID) (team1 ++ team2)
			newFb				= fb { effect = Just (GainedBall Success)}

		{-	Rules for kicking ball:
			(1) kicking decreases stamina
			(2) kicking is more effective towards your direction, and least effective in opposite direction
			(3) being taller, you can kick harder
			(4) a low stamina/health lower your max kickspeed
			(5) todo: kicking a ball held/gained by a keeper, may damage the keeper
		-}
		performAction' (KickBall (Speed3D{vxy=Speed{velocity=v,direction=d},vz=vz}),playerID) (seed,ball,team1,team2) = runIdentity $ do
			(team1,team2)		← return $ splitAt (length team1) (unbreak1 (uneq1,newFb,uneq2))
			return $ (seed2,Free newBall,team1,team2)
			where
			(uneq1,fb,uneq2)	= break1 (identifyPlayer playerID) (team1 ++ team2)
			(p1,seed1)			= random seed
			(p2,seed2)			= random   seed1
			newFb				= fb { stamina=newStamina,effect=Just (KickedBall (Just newSpeed))}
			theBall				= getBall ball (team1 ++ team2)
			skills				= skillsAsList fb
			fatHealth			= getHealthStaminaFactor (health fb) (stamina fb)
			maxV				= maxVelocityBallKick fb
			newV				= speedFactor * (v `boundedBy` (0,maxV))
			newVz				= speedFactor * (vz `boundedBy` (0, maxV))
			newSpeed			= Speed3D{vxy=Speed{velocity=newV,direction=newDirection},vz=newVz}
			newStamina			= kickingPenalty fb newV * (stamina fb)
			speedFactor		= oppositeKickPenalty fb d
			newBall			= theBall { ballSpeed=newSpeed}
			newDirection = runIdentity $ do
				if p2 == (fromIntegral 1)		then return d else do
					failure		← return $ (fromIntegral 1) - if (elem Kicking skills) then (makeRandomRealisticSkilled p2) else (makeRandomRealistic p2)
					if p1 `mod` (2::Int) ≡ 0 then do
							newD		← return $ d - failure * maxKickingDeviation fb
							return $ if (newD < (zero)) then (newD + 2.0*pi) else newD
						else do
							newD		← return $ d + failure * maxKickingDeviation fb
							return $ if (newD > 2.0*pi) then (newD - 2.0*pi) else newD

		{-	Rules for heading ball:
			(1) heading decreases stamina, but less than kicking
			(2) kicking is more effective towards your direction, and least effective in opposite direction
			(3) a low stamina/health lower your max headspeed, but less than kicking
			(4) heading is less harder than kicking, but is not effected by your length
			(5) todo: heading a ball held/gained by a keeper, may damage the keeper (less than with kicking)
		-}
		performAction' (HeadBall (Speed3D{vxy=Speed{velocity=v,direction=d},vz=vz}),playerID) (seed,ballstate,team1,team2) = runIdentity $ do
			(team1,team2)		← return $ splitAt (length team1) (unbreak1 (uneq1,newFb,uneq2))
			return $ (seed2,Free newBall,team1,team2)
			where
			(uneq1,fb,uneq2)	= break1 (identifyPlayer playerID) (team1 ++ team2)
			(p1,seed1)			= random seed
			(p2,seed2)			= random   seed1
			skills				= skillsAsList fb
			fatHealth			= getHealthStaminaFactor (health fb) (stamina fb)
			ball				= getBall ballstate (team1 ++ team2)
			ballSpeed'			= (velocity (vxy (ballSpeed ball)))
			maxV				= maxVelocityBallHead fb ballSpeed'
			newV				= v `boundedBy` (zero, maxV)
			newVz				= 0.25 * (vz `boundedBy` (0, maxV))
			newDirection		 = runIdentity $ do
				if p2 == (fromIntegral 1)	 then return d else do
					failure		← return $ (fromIntegral 1) - if (elem Heading skills) then makeRandomRealisticSkilled p2 else makeRandomRealistic p2
					if p1 `mod` (2::Int) ≡ 0 then do
							newD		← return $ d - failure * maxHeadingDeviation fb
							return $ if (newD < (zero)) then (newD + 2.0*pi) else newD
						else do
							newD		← return $ d + failure * maxHeadingDeviation fb
							return $ if (newD > 2.0*pi) then (newD - 2.0*pi) else newD
			newSpeed			= Speed3D{vxy=Speed{velocity=newV,direction=newDirection},vz=newVz}
			newStamina			= headingPenalty fb newV ballSpeed' * (stamina fb)
			newFb				= fb { stamina=newStamina,effect=Just (HeadedBall (Just newSpeed))}
			newBall			= ball { ballSpeed=newSpeed}

		{-	Rules for feinting:
			(1) you must have velocity in order to feint manouvre.
			(2) a feint manouvre changes your position, and decreases your velocity (depends on Feinting skill)
		-}
		performAction' (Feint d,playerID) (seed,ball,team1,team2) = runIdentity $ do
			(team1,team2)		← return $ splitAt (length team1) (unbreak1 (uneq1,newFb,uneq2))
			return $ (seed,ball,team1,team2)
			where
			(uneq1,fb,uneq2)	= break1 (identifyPlayer playerID) (team1 ++ team2)
			playerHasBall		= ballIsGainedBy playerID ball
			newStamina			= maxFatigueLossAtFeint fb * (stamina fb)
			fatHealth			= getHealthStaminaFactor (health fb) (stamina fb)
			newVelocity		= fatHealth * (velocity (speed fb)) * maxVelocityLossAtFeint fb
			newSpeed			= (speed fb) { velocity=newVelocity}
			(leftv,rightv)		= orthogonal (direction (speed fb))
			sidestep			= case d of FeintLeft → leftv; _ → rightv
			newPosition'		= movePoint ((scaleVector (maxFeintStep fb) (toRVector sidestep))
				                                              +
				                               (scaleVector (unittime * newVelocity) (toRVector (direction (speed fb))))
				                              ) (pos fb)
			newPosition		= pointToRectangle ((zero),Position{px=(flength theField),py=(fwidth theField)}) newPosition'
			newFb				= fb { pos=newPosition,speed=newSpeed,stamina=newStamina,effect=Just (Feinted d)}

		{- Rules for Tackling
			(1) tackling may lower the health of the victim but increases his stamina (last is because he lies on the ground the next rounds)
			(2) tackling costs stamina
		-}
		performAction' (Tackle victimID ve,playerID) (seed,ball,team1,team2) = runIdentity $ do
			return $ (seed1,newBall,team1T,team2T)
			where
			nrPlayersTeam1		= length team1
			(uneq1,fb,uneq2)	= break1 (identifyPlayer playerID) (team1 ++ team2)
			(team1N,team2N)		= splitAt nrPlayersTeam1 (unbreak1 (uneq1,newFb,uneq2))
			(uneq1T,fbT,uneq2T)	= break1 (identifyPlayer victimID) (team1N ++ team2N)
			(team1T,team2T)		= splitAt nrPlayersTeam1 (unbreak1 (uneq1T,newTarget,uneq2T))
			newStaminaSelf	= maxFatigueLossAtTackle fb * (stamina fb)
			fatHealthSelf		= getHealthStaminaFactor (health fb) (stamina fb)
			newFb				= fb { stamina = newStaminaSelf, effect = Just (Tackled victimID ve Success)}
			targetHasBall		= ballIsGainedBy victimID ball
			(p,seed1)			= random seed
			newV'				= min maxTackleVelocity ve
			maxTackleVelocity	= 10.0
			newV				= newV'/10.0
			healthDamageTarget	= newV * fatHealthSelf * (0.5*p + 0.1) + ((height fbT)-minLength)/2.0
			newHealthTarget	= max 0.0 (health fbT) - healthDamageTarget
			newTarget			= fbT { health = newHealthTarget, effect = Just (OnTheGround 3) }
			newBall			= if targetHasBall then (Free (mkBall (pos fbT) (speed fbT))) else ball

		{- Rules for Schwalbe
			(1) Schwalbe cures stamina
			(2) Performing a Schwalbe when ball was gained causes to lose ball
		-}
		performAction' (Schwalbe,playerID) (seed,ball,team1,team2) = runIdentity $ do
			(team1,team2)		← return $ splitAt (length team1) (unbreak1 (uneq1,newFb,uneq2))
			return $ (seed,ball,team1,team2)
			where
			(uneq1,fb,uneq2)	= break1 (identifyPlayer playerID) (team1 ++ team2)
			newFb				= fb { effect = Just (OnTheGround 1)}

		{- Rules for catching
			(1) ball optains speed and distance of player
		-}
		performAction' (CatchBall,playerID) (seed,ball,team1,team2) = runIdentity $ do
			(team1,team2)		← return $ splitAt (length team1) (unbreak1 (uneq1,newFb,uneq2))
			return $ (seed,GainedBy playerID,team1,team2)
			where
			(uneq1,fb,uneq2)	= break1 (identifyPlayer playerID) (team1 ++ team2)
			newFb				= fb { effect=Just (CaughtBall Success)}

		{- Rules for playing theater
			(1) playingTheater costs stamina
			(2) Performing a Schwalbe when ball was gained causes to lose ball
		-}
		performAction' (PlayTheater,playerID) (seed,ball,team1,team2) = runIdentity $ do
			(team1,team2)		← return $ splitAt (length team1) (unbreak1 (uneq1,newFb,uneq2))
			return $ (seed,ball,team1,team2)
			where
			(uneq1,fb,uneq2)	= break1 (identifyPlayer playerID) (team1 ++ team2)
			e				= (effect fb)
			wasOnTheGround		= if (isNothing e) then False else (isOnTheGround (fromJust e))
			newEvent			= if (isOnTheGround (fromJust e)) then (fromJust e) else PlayedTheater
			newFb				= fb { effect = Just newEvent}

	moveBumpedPlayers ∷ Speed → [PlayerID] → (StdGen,[Player],[Player])
											  → (StdGen,[Player],[Player])
	moveBumpedPlayers newSpeed bumpedInto (seed,team1,team2)
		= foldl (moveBumpedPlayer newSpeed) (seed,team1,team2) bumpedInto
		where
		moveBumpedPlayer ∷ Speed → (StdGen,[Player],[Player]) → PlayerID
								 → (StdGen,[Player],[Player])
		moveBumpedPlayer newSpeed (seed,team1,team2) playerID = runIdentity $ do
			(team1,team2)		← return $ splitAt (length team1) (unbreak1 (uneq1,newFb,uneq2))
			return $ (seed1,team1,team2)
			where
			(uneq1,fb,uneq2)	= break1 (identifyPlayer playerID) (team1 ++ team2)
			(p,seed1)			= random seed
			newPos				= let a = p*2.0*pi in movePoint RVector {dx=0.5*cos a,dy=0.5*sin a} (pos fb)
			newFb				= fb { speed=newSpeed, pos=newPos}


	{-	moveBall moves the ball (fromIntegral 1) unit, taking into account the surface and air resistance.
	-}
	moveBall ∷ Field → [Player] → (BallState,StdGen) → (BallState,StdGen)
	moveBall _ _ gained@(GainedBy playerID,seed)
		= gained
	moveBall field allPlayers (Free ball@Ball{ballSpeed=Speed3D{vxy=Speed{velocity=v,direction=d},vz=vz},ballPos=ballPos},seed)
		= (Free ball { ballSpeed=newSpeed,ballPos=newBallpos},seed1)
		where
		inTheAir				= (pz ballPos) > (zero)
		resistance				= if inTheAir then airResistance else surfaceResistance
		surfaceMovement		= scaleVector (unittime * v) (toRVector d)
		newSpeed2D				= let newV = resistance*v in Speed{direction = d, velocity = if (newV <= 0.05) then (zero) else newV}
--			newVz'					= if inTheAir then (vz - unittime*accellerationSec) else (zero)
		newVz'					= vz - unittime*accellerationSec
		newHeight'				= (pz ballPos) + vz
		(newHeight,newVz)		= if (newHeight' < (zero)) then (0.5*(abs newHeight'),let newV = 0.33*(abs newVz') in if (newV <= 0.8) then (zero) else newV) else	-- ball bounces, loss of velocity
									                       (newHeight',newVz')
		newSpeed'				= Speed3D{vxy=newSpeed2D, vz=newVz}
		newBallpos				= Position3D{pxy=movePoint surfaceMovement (pxy ballPos),pz=newHeight}
		fieldDimensions		= ((zero),Position{px=(flength field),py=(fwidth field)})
		(newSpeed,seed1)		= ballBouncesAgainst field newBallpos newSpeed' allPlayers seed

		-- if the then ball else bounces against something, velocity will be reduced again and the direction will be changed.
		ballBouncesAgainst ∷ Field → Position3D → Speed3D → [Player] → StdGen → (Speed3D,StdGen)
		ballBouncesAgainst field newBallpos newSpeed@Speed3D{vxy=Speed{velocity=v,direction=d},vz=s3d} allPlayers seed
			-- the ball may hit (fromIntegral 1) of the poles of (fromIntegral 1) of the goal or (fromIntegral 1) of the players and bounce away
			-- ball hits (fromIntegral 1) of the goal poles
			| againstGoalWestNorthPole || againstGoalWestSouthPole || againstGoalEastNorthPole || againstGoalEastSouthPole
				-- 50% bounce left, 50% bounce right
				= (newSpeed { vxy = (vxy newSpeed) { direction = if (p1<=0.5) then (d-p2*pi) else (d+p2*pi), velocity = resistance*v}},seed2)
			-- ball hits the top of (fromIntegral 1) of the goals
			| againstGoalWestPoleUpper || againstGoalEastPoleUpper = runIdentity $ do
				forwardOrBack	← return $ if (p1<=0.5) then Forward else Back
				upOrDown		← return $ if ((pz newBallpos) < goalHeight+(goalPoleWidth/2.0)) then Down else Up
				return $ bounceBall upOrDown (bounceBall forwardOrBack (newSpeed,seed2))
			-- ball hits (fromIntegral 1) of the players
			| any (\fb → inRadiusOfPlayer (pxy newBallpos) fb && (height fb) >= (pz newBallpos)) allPlayers
				-- bounces pure at random (player might get ported)
				= (newSpeed { vxy = (vxy newSpeed) { direction = p2*2.0*pi, velocity = resistance*v}, vz=p1*s3d},seed2)
			-- ball hits nothing
			| otherwise
				= (newSpeed,seed2)
			where
			(p1,seed1)					= random seed
			(p2,seed2)					= random seed1
			(northPole,southPole)		= goalPoles field
			againstGoalWestNorthPole 	= inCircleRadiusOfPosition newBallpos (goalPoleWidth/2.0) goalHeight Position {px=0.0,          py=northPole - goalPoleWidth/2.0}
			againstGoalWestSouthPole	= inCircleRadiusOfPosition newBallpos (goalPoleWidth/2.0) goalHeight Position {px=0.0,          py=southPole + goalPoleWidth/2.0}
			againstGoalEastNorthPole	= inCircleRadiusOfPosition newBallpos (goalPoleWidth/2.0) goalHeight Position {px=(flength field),py=northPole - goalPoleWidth/2.0}
			againstGoalEastSouthPole	= inCircleRadiusOfPosition newBallpos (goalPoleWidth/2.0) goalHeight Position {px=(flength field),py=southPole + goalPoleWidth/2.0}
			againstGoalWestPoleUpper	= (isbetween (py (pxy newBallpos)) (northPole-goalPoleWidth/2.0) (southPole+goalPoleWidth/2.0))
												&&
										  (isbetween (pz newBallpos) goalHeight (goalHeight+goalPoleWidth))
												&&
										  ((px (pxy newBallpos)) <= (zero))
			againstGoalEastPoleUpper	= (isbetween (py (pxy newBallpos)) (northPole-goalPoleWidth/2.0) (southPole+goalPoleWidth/2.0))
												&&
										  (isbetween (pz newBallpos) goalHeight (goalHeight+goalPoleWidth))
												&&
										  ((px (pxy newBallpos)) >= (flength field))

advanceTime ∷ Match → Match
advanceTime match@Match{playingTime=playingTime, unittime=unittime}
	= match { playingTime = max (zero) ((playingTime*160.0 - unittime)/160.0)}


{-|	Attribute altering functions depending on angles:
	params:
		Angle ∷ between (zero) and pi, how much the player is running backwards (pi is backwards).
		Angle ∷ between (zero) and pi, the difference between the desired angle and the angle the player previously ran to.
-}
alterStamina ∷ BallState → Player → Angle → Angle → Stamina
alterStamina ballState fb angleDifficulty angleDifference
	| v <= rfv											-- increase stamina
		= if s < minimumFatigue	then minimumFatigue else s**0.8
	| otherwise													-- lower stamina
		= if s > maximumFatigue	then maximumFatigue * ((fromIntegral 1) - angleDifficulty/(4.0*pi)) else fv * ((fromIntegral 1) - angleDifficulty/(4.0*pi))
	where
	v						= (velocity (speed fb))
	h							= (height fb)
	s							= (stamina fb)
	rfv								= restoreStaminaVelocity ballState fb angleDifficulty angleDifference
	diff							= v-rfv
	fv								= if (diff >= 6.0) then (s**(s**(1.6 + 2.0*h/100.0))) else
									 (if (diff >= 4.0) then (s**(1.5 + h/100.0)) else
									 (if (diff >= 2.0) then (s**(1.4 - h/100.0)) else
									                   (s**(1.3 - 2.0*h/100.0))))

alterHealth ∷ BallState → Player → Float → Angle → Angle → Health
alterHealth ballState fb p angleDifficulty angleDifference
	| (velocity (speed fb)) <= rfv		= min (h+p/10.0) 1.0	-- increase health
	| otherwise						= h
	where
	h							= (health fb)
	rfv								= restoreStaminaVelocity ballState fb angleDifficulty angleDifference

restoreStaminaVelocity ∷ BallState → Player → Angle → Angle → Velocity
restoreStaminaVelocity ballState fb angleDifficulty angleDifference
	| ballIsGainedBy (playerID fb) ballState
									= maxV / (if (elem Running   skills) then 1.6 else 2.6)
	| elem Running skills		= maxV / (if (elem Dribbling skills) then 2.0 else 3.0) * 1.22
	| otherwise						= maxV / (if (elem Dribbling skills) then 2.0 else 3.0)
	where
	skills							= skillsAsList fb
	maxV							= maxVelocity fb angleDifficulty angleDifference

maxVelocity ∷ Player → Angle → Angle → Velocity
maxVelocity fb angleDifficulty angleDifference
	= dribblingPenalty * runningPenalty * baseVelocity
	where
	skills							= skillsAsList fb
	baseVelocity					= 10.0
	dribblingPenalty				= if (elem Dribbling skills) then 0.95 else 0.85
	runningPenalty					= if (elem Running   skills) then 1.0 else  0.85

minimumFatigue						= 0.05
maximumFatigue						= 0.985


{-|	The functions below defines the penalty factor: values between 0.0 and 1.0 that define the loss of an attribute of an action.
-}
type PenaltyFactor = Float			-- a value between 0.0 and 1.0

kickingPenalty ∷ Player → Velocity → PenaltyFactor
kickingPenalty fb newV				= 1.0 - (if (elem Kicking (skillsAsList fb)) then 0.3 else 0.6) * (newV/maxV)**2.0
	where
	maxV							= maxVelocityBallKick fb

headingPenalty ∷ Player → Velocity → Velocity → PenaltyFactor
headingPenalty fb newV ballV		= 1.0 - (if (elem Heading (skillsAsList fb)) then 0.08 else 0.13) * (newV/maxV)**2.0
	where
	maxV							= maxVelocityBallHead fb ballV

maxFatigueLossAtTackle ∷ Player → PenaltyFactor
maxFatigueLossAtTackle fb			= if (elem Tackling (skillsAsList fb)) then 0.99 else 0.9

maxFatigueLossAtFeint ∷ Player → PenaltyFactor
maxFatigueLossAtFeint fb			= if (elem Feinting (skillsAsList fb)) then 0.92 else 0.77

maxVelocityLossAtFeint ∷ Player → PenaltyFactor
maxVelocityLossAtFeint fb			= if (elem Feinting (skillsAsList fb)) then 0.99 else 0.75

oppositeKickPenalty ∷ Player → Angle → PenaltyFactor
oppositeKickPenalty fb kickTo		= 1.0 - skillPenaltyFactor * (angleHowFarFromPi angle)/pi
	where
	angle							= abs ((nose fb) - kickTo)
	skills							= skillsAsList fb
	skillPenaltyFactor				= if (all (`elem` skills) [Rotating,Kicking]) then 0.3
									 else (if (any (`elem` skills) [Rotating,Kicking]) then 0.5
									 else                                             0.9)
