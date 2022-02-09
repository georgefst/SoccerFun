{-# LANGUAGE UnicodeSyntax #-}
module SoccerFun.Random where

makeRandomRealistic ∷ Floating f ⇒ f → f
makeRandomRealistic r = 1.0-r**4.0

makeRandomRealisticSkilled ∷ Floating f ⇒ f → f
makeRandomRealisticSkilled r= 1.0-r**10.0
