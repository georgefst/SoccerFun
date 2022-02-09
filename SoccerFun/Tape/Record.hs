{-# LANGUAGE UnicodeSyntax #-}
-- | Usage: Hit /q/ to abort the match
module Main where

import Control.Monad
import System.Exit
import Prelude.Unicode
import Data.Binary
import SoccerFun.Tape
import SoccerFun.MatchControl
import SoccerFun.MatchGame
import System.Environment
import Paths_SoccerFun (getDataFileName)
import System.Cmd

dynRecord ∷ FilePath → FilePath → IO ()
dynRecord p1 p2 = do
	(loc1,name1) ← compileTeam p1
	(loc2,name2) ← compileTeam p2
	record ← getDataFileName "SoccerFun/Tape/Record/Template.hs"
	exitCode ← system $ "runhaskell -i" ⧺ loc1 ⧺ " -i" ⧺ loc2 ⧺ " -DTEAM1=" ⧺ name1 ⧺ " -DTEAM2=" ⧺ name2 ⧺ " " ⧺ record
	when (exitCode ≢ ExitSuccess) (fail "Could merge teams, probably due to a type error.")

main ∷ IO ()
main = do
	prog ← getProgName
	args ← getArgs
	let (t1,t2) = case args of
		[t1,t2] → (t1,t2)
		notTwo → error $ unlines
			["Record a match between two teams <Team1> and <Team2>",
			 "Usage: " ⧺ prog ⧺ " <dir1> <dir2>",
			 "  where both arguments are paths to directories containing a team module."]
	dynRecord t1 t2

--	(t1name,t2name,match) ← dynSetupMatch t1 t2
--	let tape = recordMatch match
--	encodeFile (t1name ⧺ "-" ⧺ t2name ⧺ ".sft") tape
--	let Tape steps = tape
--	    endOfMatch = snd (last steps)
--	    finalScore = score endOfMatch
--	putStrLn $ show (fst finalScore) ⧺ " / " ⧺ show (snd finalScore)
