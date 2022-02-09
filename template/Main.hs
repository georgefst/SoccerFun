{-# LANGUAGE UnicodeSyntax #-}
module Main where

import SoccerFun.UI.GL
import Children.Team as Children

main ∷ IO ()
main = runMatch Children.team Children.team
