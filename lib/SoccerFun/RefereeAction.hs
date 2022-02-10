{-# LANGUAGE UnicodeSyntax #-}
module SoccerFun.RefereeAction where

import SoccerFun.Types
import SoccerFun.Geometry
import SoccerFun.Field

data RefereeAction
    = ReprimandPlayer PlayerID Reprimand -- ^ player with given name receives reprimand
    | Hands PlayerID                     -- ^ person is seen for doing hands
    | TackleDetected PlayerID            -- ^ person is seen for doing tackle
    | SchwalbeDetected PlayerID          -- ^ person is seen for doing schwalbe
    | TheaterDetected PlayerID
    | DangerousPlay PlayerID             -- ^ person is seen for doing dangerous actions
    | GameOver                           -- ^ end of game
    | PauseGame                          -- ^ game is paused
    | AddTime ExtraTime                  -- ^ extra time is added to the game
    | EndHalf                            -- ^ first half is over, teams go for a second half
    | Goal ATeam                         -- ^ team playing at home has scored
    | Offside PlayerID                   -- ^ player is offside at Home
    | DirectFreeKick ATeam Position      -- ^ a direct free kick is granted for team home at given position
    | GoalKick ATeam                     -- ^ a goal kick is granted for team home
    | Corner ATeam Edge                  -- ^ a corner kick is granted for team home
    | ThrowIn ATeam Position             -- ^ a throw in ball is granted for team home at given position
    | Penalty ATeam                      -- ^ penalty at homeside
    | CenterKick ATeam                   -- ^ team playing at home may start from the center
    | Advantage ATeam                    -- ^ referee gives advantages to home-team
    | OwnBallIllegally PlayerID          -- ^ ball was for the other team
    | DisplacePlayers Displacements      -- ^ displaces all players at the provided position (used with free kicks)
    | ContinueGame
    | TellMessage String                 -- ^ no effect on match, message is displayed by referee
    deriving (Show, Eq)

isReprimandPlayer ∷ RefereeAction → Bool
isReprimandPlayer (ReprimandPlayer _ _) = True
isReprimandPlayer _ = False

isHands ∷ RefereeAction → Bool
isHands (Hands _) = True
isHands _ = False

isTackleDetected ∷ RefereeAction → Bool
isTackleDetected (TackleDetected _) = True
isTackleDetected _ = False

isSchwalbeDetected ∷ RefereeAction → Bool
isSchwalbeDetected (SchwalbeDetected _) = True
isSchwalbeDetected _ = False

isTheaterDetected ∷ RefereeAction → Bool
isTheaterDetected (TheaterDetected _) = True
isTheaterDetected _ = False

isDangerousPlay ∷ RefereeAction → Bool
isDangerousPlay (DangerousPlay _) = True
isDangerousPlay _ = False

isGameOver ∷ RefereeAction → Bool
isGameOver GameOver = True
isGameOver _ = False

isPauseGame ∷ RefereeAction → Bool
isPauseGame PauseGame = True
isPauseGame _ = False

isAddTime ∷ RefereeAction → Bool
isAddTime (AddTime _) = True
isAddTime _ = False

isEndHalf ∷ RefereeAction → Bool
isEndHalf EndHalf = True
isEndHalf _ = False

isGoal ∷ RefereeAction → Bool
isGoal (Goal _) = True
isGoal _ = False

isOffside ∷ RefereeAction → Bool
isOffside (Offside _) = True
isOffside _ = False

isDirectFreeKick ∷ RefereeAction → Bool
isDirectFreeKick (DirectFreeKick _ _ ) = True
isDirectFreeKick _ = False

isGoalKick ∷ RefereeAction → Bool
isGoalKick (GoalKick _) = True
isGoalKick _ = False

isCorner ∷ RefereeAction → Bool
isCorner (Corner _ _) = True
isCorner _ = False

isThrowIn ∷ RefereeAction → Bool
isThrowIn (ThrowIn _ _) = True
isThrowIn _ = False

isPenalty ∷ RefereeAction → Bool
isPenalty (Penalty _) = True
isPenalty _ = False

isCenterKick ∷ RefereeAction → Bool
isCenterKick (CenterKick _) = True
isCenterKick _ = False

isAdvantage ∷ RefereeAction → Bool
isAdvantage (Advantage _) = True
isAdvantage _ = False

isOwnBallIllegally ∷ RefereeAction → Bool
isOwnBallIllegally (OwnBallIllegally _) = True
isOwnBallIllegally _ = False

isDisplacePlayers ∷ RefereeAction → Bool
isDisplacePlayers (DisplacePlayers _) = True
isDisplacePlayers _ = False

isContinueGame ∷ RefereeAction → Bool
isContinueGame ContinueGame = True
isContinueGame _ = False

isTellMessage ∷ RefereeAction → Bool
isTellMessage (TellMessage _) = True
isTellMessage _ = False


isGoal4ATeam ∷ ATeam → RefereeAction → Bool
isGoal4ATeam t (Goal t') = t == t'
isGoal4ATeam _ _ = False

-- | Position of a referee-granted action like a throw-in, a corner kick, etc.
getKickPos ∷ Field → Half → RefereeAction → Maybe Position
getKickPos field half (GoalKick team) = Just $ Position { py = (fwidth field)/2.0
                                                   , px = if (team == Team1 && half == FirstHalf || team == Team2 && half == SecondHalf)
                                                             then 5
                                                             else (flength field) - 5 }
getKickPos field half (Corner team edge) = Just $ Position { px = if (team == Team1 && half == SecondHalf || team == Team2 && half == FirstHalf)
                                                             then halfRadiusCornerKickArea
                                                             else ((flength field) - halfRadiusCornerKickArea)
                                                   , py = if (edge == North)
                                                             then halfRadiusCornerKickArea
                                                             else ((fwidth field) - halfRadiusCornerKickArea)
                                                   }
    where
    halfRadiusCornerKickArea = radiusCornerKickArea / 2.0
getKickPos field half (Penalty team) = Just $ Position { py = (fwidth field)/2.0
                                                   , px = if (team == Team1 && half == SecondHalf || team == Team2 && half == FirstHalf)
                                                             then penaltySpotDepth
                                                             else ((flength field) - penaltySpotDepth)
                                                   }
getKickPos field _ (CenterKick _) = Just $ Position { px = (flength field)/2.0
                                                   , py = (fwidth field) /2.0
                                                   }
getKickPos _ _ (DirectFreeKick _ pos) = Just pos
getKickPos _ _ (ThrowIn _ pos) = Just pos
getKickPos _ _ _ = Nothing
