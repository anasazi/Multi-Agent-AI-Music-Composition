import qualified FirstSpecies
import qualified GeneralSpecies

import Control
import Agent
import Blackboard

import Note
import Scale
import Voice

import System.Random
import qualified Data.Map as M

cantusfirmus = atStart [ makeNote D 3 0 0 0
                       , makeNote F 3 0 0 0
                       , makeNote G 3 0 0 0
                       , makeNote A 3 0 0 0
                       , makeNote G 3 0 0 0
                       , makeNote F 3 0 0 0
                       , makeNote E 3 0 0 0
                       , makeNote D 3 0 0 0
                       , makeNote F 3 0 0 0
                       , makeNote G 3 0 0 0
                       , makeNote E 3 0 0 0
                       , makeNote D 3 0 0 0
                       ]

allAgents = GeneralSpecies.agents ++ FirstSpecies.agents

control gen = makeControl allAgents major cantusfirmus gen

prettyPrint b cxt = do  putStrLn "Cantus Firmus"
                        print (focus (front (cantusFirmus b)))
                        putStrLn "Counter Point"
                        print (focus (front (counterPoint b)))
                        putStrLn "Hard Violations"
                        print (hard cxt)
                        putStrLn "Soft Violations"
                        print (soft cxt)

main = do 
          gen <- getStdGen
          let cc = controlLoop (control gen)
          let best = bestBlackboard (blackboards cc)
          let bestCxt = (blackboards cc) M.! best
          bestCxt `seq` prettyPrint best bestCxt
