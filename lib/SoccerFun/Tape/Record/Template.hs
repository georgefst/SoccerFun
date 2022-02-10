{-# LANGUAGE UnicodeSyntax, CPP #-}
{-# OPTIONS_GHC -pgmP cpp #-}
module Main where

import Control.Monad
import System.Exit
import Prelude.Unicode
import Data.Binary
import SoccerFun.Tape
import SoccerFun.MatchControl
import SoccerFun.MatchGame
import System.Environment
import qualified TEAM1.Team as Team1
import qualified TEAM2.Team as Team2

#define MKSTRING(x) #x
#define TOSTRING(x) MKSTRING(x)

main ∷ IO ()
main = do
    tape ← liftM recordMatch $ setupMatch Team1.team Team2.team
    encodeFile (TOSTRING(TEAM1) ⧺ "-" ⧺ TOSTRING(TEAM2) ⧺ ".sft") tape
    let Tape steps = tape
        endOfMatch = snd (last steps)
        finalScore = score endOfMatch
    putStrLn $ show (fst finalScore) ⧺ " / " ⧺ show (snd finalScore)
