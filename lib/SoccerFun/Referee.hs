{-# LANGUAGE UnicodeSyntax, ExistentialQuantification #-}
{-| The referee data type, and all available referees within Soccer-Fun.
-}
module SoccerFun.Referee (module SoccerFun.Referee, module SoccerFun.RefereeAction) where

import SoccerFun.Team
import SoccerFun.Types
import SoccerFun.Ball
import SoccerFun.Field
import SoccerFun.Geometry
import SoccerFun.RefereeAction
import System.Random
import Control.Monad.State
import Control.Monad.Identity

data Referee = ∀memory. Referee { rname ∷ String
                            , rbrain ∷ Brain (RefereeAI (memory,StdGen)) memory
--                            , refActionPics ∷ [FilePath]
                            }

instance Show Referee where
    show r = rname r

cloneReferee ∷ Brain (RefereeAI (m,StdGen)) m → Referee → Referee
cloneReferee brain (Referee rname rbrain {-refActionPics-})
    = Referee rname brain {-refActionPics-}

type RefereeBrain memory = (memory,RefereeAI (memory,StdGen))
type RefereeAI state = PlayingTime → TimeUnit
                            → BallState
                            → Half
                            → Team
                            → Team
                            → state
                            → ([RefereeAction],state)

defaultReferee ∷ Referee
defaultReferee = Referee { rname = "Default"
                          , rbrain = Brain {m = undefined, ai = \_ _ _ _ _ _ st → ([ContinueGame],st)}
--                          , refActionPics = []
                          }

--allAvailableReferees∷ [Field → Referee]
--allAvailableReferees = [ ivanovReferee ]
-- When coding for all referees, use following list:
--                            ++
--                          [ RefereeCoachslalom
--                          , RefereeCoachpassing
--                          , RefereeCoachdeepPass
--                          , RefereeCoachkeeper
--                          ]
--defaultImage ∷ FileSystem env ⇒ Match → RefereeAction → env → (Bitmap,env)
--defaultSoundFile ∷ RefereeAction → Maybe String

{-| Wrapper functions for simpler referee brains:
-}
randomlessRefereeAI ∷ (RefereeAI memory) → RefereeAI (memory,StdGen)
randomlessRefereeAI brainf = randomless brainf where
    randomless brainf playingtime unit maybeBall half team1 team2 (memory,seed) = runIdentity $ do
        (decisions,memory)    ← return $ brainf playingtime unit maybeBall half team1 team2 memory
        return $ (decisions,(memory,seed))

amnesiaRefereeAI ∷ (RefereeAI StdGen) → RefereeAI (memory,StdGen)
amnesiaRefereeAI brainf = amnesia brainf where
    amnesia brainf playingtime unit maybeBall half team1 team2 (memory,seed) = runIdentity $ do
        (decisions,seed)      ← return $ brainf playingtime unit maybeBall half team1 team2 seed
        return (decisions,(memory,seed))


witlessRefereeAI ∷ (RefereeAI a) → RefereeAI (memory,StdGen)
witlessRefereeAI brainf = witless brainf where
    witless brainf playingtime unit maybeBall half team1 team2 state =
        (fst (brainf playingtime unit maybeBall half team1 team2 undefined),state)


--import matchGame
--import Ivanov (ivanovReferee)
-- When coding for all referees, include following modules:
--import RefereeCoachslalomassignment
--import RefereeCoachpassingassignment
--import RefereeCoachdeepPassassignment
--import RefereeCoachkeeperassignment


instance NameOf Referee where nameOf r = rname r


--defaultImage ∷  FileSystem env ⇒ Match RefereeAction env → (Bitmap,env)
--defaultImage match rev env = let bitmapf = case rev of
--                            (ReprimandPlayer _ Warning) → "ivanovWarning.bmp"
--                            (ReprimandPlayer _ YellowCard) → "ivanovYellow.bmp"
--                            (ReprimandPlayer _ RedCard) → "ivanovRed.bmp"
--                            (Hands _) → "hands.bmp"
--                            (OwnBallIllegally _) → "ivanovBadluck.bmp"
--                            (TellMessage _) → "ivanovLook.bmp"
--                            (DirectFreeKick _ p) → if (p.px < match.theField.flength/2.0) "ivanovWijstLinks.bmp" "ivanovWijstRechts.bmp"
--                            (GoalKick t) → if (pointLeft match t) "ivanovWijstLinks.bmp" "ivanovWijstRechts.bmp"
--                            (Corner t _) → if (pointLeft match t) "ivanovWijstLinks.bmp" "ivanovWijstRechts.bmp"
--                            (ThrowIn t _) → if (pointLeft match t) "ivanovWijstLinks.bmp" "ivanovWijstRechts.bmp"
--                            (Penalty t) → if (pointLeft match t) "ivanovWijstLinks.bmp" "ivanovWijstRechts.bmp"
--                            (Advantage _) → "ivanovBadluck.bmp"
--                            (TheaterDetected _) → "ivanovTheater.bmp"
--                            _ → "ivanovFluit.bmp"
--    in case openBitmap ("afbeeldingen\\"+++bitmapf) env of
--        (Just bm,env) = (bm,env)
--        nothing = abort "defaultImage: unable to load default picture.\n"
--    where
--    pointLeft match t = match.playingHalf == FirstHalf && t == Team1 || match.playingHalf == SecondHalf && t == Team2

--defaultSoundFile ∷ RefereeAction → Maybe String
--defaultSoundFile rev = if (soundfilename == "") Nothing (Just ("sound\\"+++soundfilename))
--    where
--    soundfilename = defaultSoundFileName rev
--
--    defaultSoundFileName (Hands _) = "stopBecauseOfFoul.wav"
--    defaultSoundFileName (TheaterDetected _) = "tacklesEd.wav"
--    defaultSoundFileName (TackleDetected _) = "tacklesEd.wav"
--    defaultSoundFileName (SchwalbeDetected _) = "tacklesEd.wav"
--    defaultSoundFileName (DangerousPlay _) = "tacklesEd.wav"
--    defaultSoundFileName GameOver = "endGameOrHalf.wav"
--    defaultSoundFileName EndHalf = "endGameOrHalf.wav"
--    defaultSoundFileName (Offside _) = "offside.wav"
--    defaultSoundFileName (GoalKick _) = "ballOut.wav"
--    defaultSoundFileName (Corner _ _) = "ballOut.wav"
--    defaultSoundFileName (ThrowIn _ _) = "ballOut.wav"
--    defaultSoundFileName (Goal _) = "CenterKick.wav"
--    defaultSoundFileName (OwnBallIllegally _) = "wrongPosition2restartFrom.wav"
--    defaultSoundFileName _ = ""
