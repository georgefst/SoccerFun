{-# LANGUAGE UnicodeSyntax #-}
module SoccerFun.MatchGame where

import Prelude.Unicode
import SoccerFun.Types
import SoccerFun.Player
import SoccerFun.MatchControl
import SoccerFun.Field
import SoccerFun.Team
import SoccerFun.RefereeAction
import SoccerFun.Prelude
import SoccerFun.Referee.Ivanov
import Control.Monad
import System.Random
import System.Exit
import System.Process
import Data.List (findIndices)


showTime ∷ Minutes → String -- ^ display time in (mm:ss min) format
showTime minutes = show (fromIntegral seconds/60) ++ ":" ++ (if (seconds `mod` 60 < 10) then "0" else "") ++ show (seconds `mod` 60) ++ " min"
    where
    seconds = round (((fromIntegral (round (minutes * 100.0) ∷ Int))/100.0) * 60.0) ∷ Int


logRefereeAction :: RefereeAction -> Maybe String
logRefereeAction (ReprimandPlayer tfp r) = Just $ "ReprimandPlayer " ++ show (playerNo tfp) ++ " " ++ show r
logRefereeAction (Hands tfp) = Just $ "Hands " ++ show (playerNo tfp)
logRefereeAction (TackleDetected tfp) = Just $ "TackleDetected " ++ show (playerNo tfp)
logRefereeAction (SchwalbeDetected tfp) = Just $ "SchwalbeDetected " ++ show (playerNo tfp)
logRefereeAction (TheaterDetected tfp) = Just $ "TheaterDetected " ++ show (playerNo tfp)
logRefereeAction (DangerousPlay tfp) = Just $ "DangerousPlay " ++ show (playerNo tfp)
logRefereeAction GameOver = Just $ "GameOver"
logRefereeAction PauseGame = Just $ "PauseGame"
logRefereeAction (AddTime t) = Just $ "AddTime " ++ (showTime t)
logRefereeAction EndHalf = Just $ "EndHalf"
logRefereeAction (Goal t) = Just $ "Goal " ++ show t
logRefereeAction (Offside tfp) = Just $ "Offside " ++ show (playerNo tfp)
logRefereeAction (DirectFreeKick t p) = Just $ "DirectFreeKick " ++ show t ++ " " ++ show p
logRefereeAction (GoalKick t) = Just $ "GoalKick " ++ show t
logRefereeAction (Corner t e) = Just $ "Corner " ++ show t ++ " " ++ show e
logRefereeAction (ThrowIn t p) = Just $ "ThrowIn " ++ show t ++ " " ++ show p
logRefereeAction (Penalty t) = Just $ "Penalty " ++ show t
logRefereeAction (CenterKick t) = Just $ "CenterKick " ++ show t
logRefereeAction (Advantage t) = Just $ "Advantage " ++ show t
logRefereeAction (OwnBallIllegally tfp) = Just $ "OwnBallIllegally" ++ show (playerNo tfp)
logRefereeAction (DisplacePlayers ds) = Nothing -- Just $ "DisplacePlayers"
logRefereeAction ContinueGame = Nothing
logRefereeAction (TellMessage txt) = Just $ "TellMessage " ++ show txt

setupMatch ∷ (Home → Field → Team) → (Home → Field → Team) → IO Match
setupMatch t1 t2 = let
        field = Field 70 105
        team1 = take 11 $ t1 West field
        team2 = take 11 $ t2 East field
    in liftM (setMatchStart team1 team2 field (ivanovReferee field team1 team2) 1) newStdGen

compileTeam ∷ FilePath → IO (FilePath,String)
compileTeam p = do
    let (loc,teamName) = dissectPath p
    let teamMod = teamName ⧺ ".Team"
    exitCode ← compile (Just loc) teamMod
    if exitCode ≡ ExitSuccess
        then return (loc,teamName)
        else fail ("Failed compiling " ⧺ teamMod ⧺ " in " ⧺ loc ⧺ ".")
    where
    dissectPath ∷ FilePath → (FilePath, String)
    dissectPath p = case findIndices isSlash t of
        [ ] → (".", t)
        ixs → let (loc,slash:name) = splitAt (last ixs) t in (loc, name)
    t = removeTrailingSlashes p
    removeTrailingSlashes str = if isSlash (last str) then removeTrailingSlashes (init str) else str
    isSlash = flip elem ['/','\\']

    compile ∷ (Maybe FilePath) → String → IO ExitCode
    compile workingDir modName = waitForProcess =<< runProcess
        "ghc" ["--make", "-v0", modName] workingDir Nothing Nothing Nothing Nothing


--    inDir dir ioAction = do
--        oldDir ← getCurrentDirectory
--        setCurrentDirectory dir
--        res ← ioAction
--        setCurrentDirectory oldDir
--        return res

{- This module defines the match and tournament data structures.
-}
--import MatchLog, GuiInterface

--data BallGame
--    = BallGame { match ∷ Match -- the ball match to be played
--        , actionPics ∷ ActionPics -- the action-images
--        , history ∷ History -- recent history of game
--        , frames ∷ Int -- nr of frames so far (reset to zero every second)
--        , options ∷ Options -- options of ball game
--        , logging ∷ WhatToLog -- logging options
--        }
--
--data Options
--    = Options { closeReferee ∷ Bool -- automatically close referee dialog after one second (True - default) or by user (False)
--        , showSplash ∷ Bool -- show splash screen at opening (False - default) or do (True)
--        , displaySpeed ∷ DisplaySpeed -- slow, normal or fast-play (Normal - default)
--        , showReferee ∷ Bool -- show referee-intermezzo (True - default) or not (False)
--        , playingTime ∷ PlayingTime -- default playingtime (defaultPlayingTime)
--        }
--instance show Options
--instance fromString Options
--instance == Options
--
--data History
--    = History { time ∷ Seconds -- time in seconds of length history
--        , past ∷ [Match] -- the recent history
--        }
--
--{- incFrames game increases the frames count of game.
---}
--incFrames ∷ BallGame → BallGame
--incFrames game@{frames} = game {frames=frames+1}
--
--{- defaultPlayingTime returns recommended playing time
---}
--defaultPlayingTime ∷ PlayingTime
--defaultPlayingTime ∷ PlayingTime
--defaultPlayingTime = 0.66--1.0
--
--{- defaultOptions returns default options.
---}
--defaultOptions ∷ Options
--defaultOptions
--    = { closeReferee = True
--      , showSplash = False
--      , displaySpeed = Normal
--      , showReferee = True
--      , playingTime = defaultPlayingTime
--      }
--
--
--{- timeLeft is True if the game has not finished
---}
--timeLeft ∷ BallGame → Bool
--timeLeft game = game.match.Match.playingTime > zero
--
--{- getOptions env reads the options file (if present) and returns its content.
--        If no options file was found, it is created and filled with default values.
--    setOptions options stores the options in the options file.
---}
--getOptions ∷ *env → (Options,*env) | FileSystem env
--getOptions env
--    = case readFile optionsFile env of
--        (Just options,env) = (fromString options,env)
--        (nothing, env) = (defaultOptions, env)
--setOptions ∷ Options *env → *env | FileSystem env
--
--data Competition = Competition { results ∷ [[Maybe Score]] -- teams x teams matrix of match results (note: team x team → Nothing)
--type Ranking = AssocList ClubName Rank
--data Rank = Rank { matchpoints ∷ Int -- number of matchpoints (>= 0)
--                                , goalsScored ∷ Int -- number of scored goals (>= 0)
--                                , goalsAgainst ∷ Int -- number of goals against (>= 0)
--                                }
--instance zero Rank
--instance == Rank
--instance < Rank
--instance + Rank
--                                , west ∷ [ClubName] -- names of participating teams (west side)
--                                , east ∷ [ClubName] -- names of participating teams (east side)
--                                , usedRandomSeed∷ RandomSeed -- the seed that is used for computing the matches
--                                }
--
--{- competition teams field referee time rs
--        computes an entire competition between all teams in teams.
--        Each match uses the same referee and same initial random seed value rs.
---}
--competition ∷ [Home Field → Team] Field Referee PlayingTime RandomSeed → Competition
--
--{- computeMatch match
--        computes an entire match between the currently selected team1 and team2.
---}
--computeMatch ∷ Match → Score
--
--{- ranking competition
--        computes the ranking of all teams that have participated in competition.
---}
--ranking ∷ Competition → Ranking
--ranking ∷ [ClubName] [Maybe Score] → Ranking
--
--{- checkCompetitionFile westTeamNames rs env
--        checks whether there is a competition backup file present for the current set
--        of teams (assuming they start on the West home side) and initial random seed value rs
--        for computing matches.
--        If not, then such a file is created, and the same random seed value and empty list of scores is returned.
--        If so, then the currently stored random seed value and list of scores is returned.
---}
--checkCompetitionFile ∷ [ClubName] RandomSeed *env → ((RandomSeed,[Maybe Score]),*env) | FileSystem env
--
--{- appendMatchToCompetitionFile west east env
--        appends an empty entry of a match between west versus east in the competition backup file.
--        It also returns the file pointer to allow a correct update in updateMatchToCompetitionFile.
---}
--appendMatchToCompetitionFile∷ ClubName ClubName *env → (Int,*env) | FileSystem env
--
--
--{- updateMatchToCompetitionFile west east score filepointer env
--        updates the line that starts at filepointer in the competition backup file with the result
--        of the match between west versus east.
---}
--updateMatchToCompetitionFile∷ ClubName ClubName (Maybe Score) Int *env → *env | FileSystem env
--
--import StdEnvExt, fileIO
--import guiInterface, matchControl
--pmort Parsers (parse, ∷ Parser, ∷ Result(..), ∷ SugPosition, ∷ Rose(..), ∷ RoseNode(..), ∷ SymbolTypes(..), ∷ SymbolType(..), yield, token, symbol, <&>, &>, <!>, <!+>, <!*>, number, digit)
--
--
--
--
--
--instance zero Rank where
--    zero = { matchpoints = zero, goalsScored = zero, goalsAgainst = zero }
--instance == Rank where
--    (==) r1 r2 = (r1.matchpoints,r1.goalsScored,r1.goalsAgainst) == (r2.matchpoints,r2.goalsScored,r2.goalsAgainst)
--instance < Rank where
--    (<) r1 r2 = r1.matchpoints < r2.matchpoints ||
--                              r1.matchpoints == r2.matchpoints && r1.goalsScored < r2.goalsScored ||
--                              r1.matchpoints == r2.matchpoints && r1.goalsScored == r2.goalsScored && r1.goalsAgainst > r2.goalsAgainst
--instance + Rank where
--    (+) r1 r2 = { matchpoints = r1.matchpoints + r2.matchpoints
--                              , goalsScored = r1.goalsScored + r2.goalsScored
--                              , goalsAgainst = r1.goalsAgainst + r2.goalsAgainst
--                              }
--
--competition ∷ [Home Field → Team] Field Referee PlayingTime RandomSeed → Competition
--competition teams field referee playingtime rs
--    = { results = [ [ if (nrWest == nrEast)
--                                      Nothing
--                                      (Just (computeMatch (setMatchStart (teamWest West field) (teamEast East field) field referee playingtime rs)))
--                                | (nrEast,teamEast) <- zip2 [1..] teams
--                                ]
--                              | (nrWest,teamWest) <- zip2 [1..] teams
--                              ]
--      , west = map (\f → nameOf (f West field)) teams
--      , east = map (\f → nameOf (f East field)) teams
--      , usedRandomSeed = rs
--      }
--

computeMatch ∷ Match → Score
computeMatch match
    | playingTime match > zero = computeMatch (snd (stepMatch match))
    | otherwise = score match

{-
ranking ∷ [ClubName] [Maybe Score] → Ranking
ranking names scores = foldl upd [(t,zero) | t <- names] (zip2 [(tw,te) | tw <- names, te <- names] scores)
    where
    upd ranking (_,Nothing)
        = ranking
    upd ranking ((west,east),Just (goalsWest,goalsEast))
        = updkeyvalue west ((+) rankWest) (updkeyvalue east ((+) rankEast) ranking)
        where
        (mpsWest, mpsEast) = if (goalsWest > goalsEast) (3,0) (if (goalsWest < goalsEast) (0,3) (1,1))
        (rankWest,rankEast) = ({matchpoints=mpsWest,goalsScored=goalsWest,goalsAgainst=goalsEast}
                                  ,{matchpoints=mpsEast,goalsScored=goalsEast,goalsAgainst=goalsWest}
                                  )

instance show Options where
    show {closeReferee,showSplash,displaySpeed,showReferee,playingTime}
                            = "{closeReferee=" ++ show closeReferee ++ show
                              ",showSplash=" ++ show showSplash ++ show
                              ",displaySpeed=" ++ show displaySpeed ++ show
                              ",showReferee=" ++ show showReferee ++ show
                              ",playingTime=" ++ show playingTime ++ show
                              "}"
instance fromString Options where
    fromString str
    = case parse optionsP (fromString str) optionsFile "char" of
        Succ [opt:_] = opt
        _ = defaultOptions
        where
        optionsP ∷ Parser Char Options Options
        optionsP = token ['{closeReferee='] &>
                              boolP <&> \closeReferee →
                              token [',showSplash='] &>
                              boolP <&> \showSplash →
                              token [',displaySpeed='] &>
                              displaySpeedP <&> \displaySpeed →
                              token [',showReferee='] &>
                              boolP <&> \showReferee →
                              token [',playingTime='] &>
                              realP <&> \playingTime →
                              symbol '}' &>
                              yield { closeReferee = closeReferee
                                    , showSplash = showSplash
                                    , displaySpeed = displaySpeed
                                    , showReferee = showReferee
                                    , playingTime = playingTime
                                    }
        boolP = (token ['True'] &> yield True) <!> (token ['False'] &> yield False)
        realP = <!*> (symbol '-') <&> \minuss →
                              <!*> digit <&> \digits1 →
                              <!*> (symbol '.') <&> \dots →
                              <!*> digit <&> \digits2 →
                              yield (toReal (show (minuss ++ digits1 ++ dots ++ digits2)))
        displaySpeedP = (token ['Slow'] &> yield Slow) <!>
                              (token ['Target'] &> yield Target) <!>
                              (token ['Normal'] &> yield Normal) <!>
                              (token ['Faster'] &> yield Faster) <!>
                              (token ['Fastest'] &> yield Fastest)

instance == Options where
    (==) o1 o2 = o1.closeReferee == o2.closeReferee &&
                              o1.showSplash == o2.showSplash &&
                              o1.displaySpeed == o2.displaySpeed &&
                              o1.showReferee == o2.showReferee &&
                              o1.Options.playingTime == o2.Options.playingTime


setOptions ∷ Options *env → *env | FileSystem env
setOptions options env = writeFile False optionsFile (show options) env

optionsFile = "SoccerFunOptions.txt"

checkCompetitionFile ∷ [ClubName] RandomSeed *env → ((RandomSeed,[Maybe Score]),*env) | FileSystem env
checkCompetitionFile west rs env
# (ok,cf,env) = fopen competitionFile FReadText env
| not ok = ((rs,[]), createCompetitionFile west rs env) -- competition file does not exist: create it
# (ok,frs,fwest,cf) = header cf
| not ok || fwest <> teamsLine west
                            = ((rs,[]), createCompetitionFile west rs (snd (fclose cf env))) -- competition file ill-formatted or different set of teams: create it
# (scores,cf) = readScores cf -- competition file exists, and for this competition
# (ok,env) = fclose cf env
| not ok = abort ("Could not close competition file after reading scores.\n" +++
                                show (length scores)
                                )
| otherwise = ((frs,scores),env)
    where
    readScores ∷ *File → ([Maybe Score],*File)
    readScores cf
    # (end,cf) = fend cf
    | end = ([],cf)
    # (line,cf) = freadline cf
    # score = if (line.[0] == 'x') Nothing
                             (let (i1,l1) = span ((<>) ' ') [c | c<-:line]
                                  (i2,l2) = span ((<>) ' ') (tl l1)
                               in Just (toInt (show i1),toInt (show i2))
                             )
    # (scores,cf) = readScores cf
    = ([score:scores],cf)

appendMatchToCompetitionFile∷ ClubName ClubName *env → (Int,*env) | FileSystem env
appendMatchToCompetitionFile west east env
# (ok,cf,env) = fopen competitionFile FAppendText env
| not ok = abort "Could not open competition file for appending data.\n"
# (pos,cf) = fposition cf
# (ok,env) = fclose (cf <<< "x " <<< west <<< " vs " <<< east <<< '\n') env
| not ok = abort "Could not close competition file after appending data.\n"
| otherwise = (pos,env)

updateMatchToCompetitionFile∷ ClubName ClubName (Maybe Score) Int *env → *env | FileSystem env
updateMatchToCompetitionFile west east score pos env
# (ok,cf,env) = fopen competitionFile FAppendText env
| not ok = abort "Could not open competition file for appending data.\n"
# (ok,cf) = fseek cf pos FSeekSet
| not ok = abort "Could not seek in competition file for updating data.\n"
# (ok,env) = fclose (cf <<< result <<< ' ' <<< west <<< " vs " <<< east <<< '\n') env
| not ok = abort "Could not close competition file after appending data.\n"
| otherwise = env
    where
    result = case score of
                                Nothing = "x"
                                Just (gw,ge) = gw +++> (" " ++ show ge)

createCompetitionFile ∷ [ClubName] RandomSeed *env → *env | FileSystem env
createCompetitionFile west rs env
# (ok,cf,env) = fopen competitionFile FWriteText env
| not ok = abort "Could not create competition file.\n"
# (ok,env) = fclose (cf <<< seedLine rs <<< '\n' <<< teamsLine west <<< '\n') env
| not ok = abort "Could not close competition file.\n"
| otherwise = env

header ∷ *File → (Bool,RandomSeed,String,*File)
header file
# (rsLine, file) = freadline file
# (teamsLine,file) = freadline file
= (size rsLine > 1 && size teamsLine > 1, fromString (rsLine%(0,size rsLine-2)), teamsLine%(0,size teamsLine-2),file)

seedLine ∷ RandomSeed → String
seedLine rs = show rs

teamsLine ∷ [ClubName] → String
teamsLine west = foldl (\t ts → t +++ "," +++ ts) "" west

competitionFile = "competition.txt"

------------------------------------------------------------------------------

-}
