import qualified FirstSpecies
import qualified GeneralSpecies

import Control
import Agent
import Blackboard

import Note
import Scale
import Voice

import System.Random

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

prettyPrint bcxt = do putStrLn "Cantus Firmus"
                      print (focus (front (cantusFirmus (board bcxt))))
                      putStrLn "Counter Point"
                      print (focus (front (counterPoint (board bcxt))))
                      putStrLn "Hard Violations"
                      print (hardViolations bcxt)
                      putStrLn "Soft Violations"
                      print (softViolations bcxt)

main = do 
          gen <- getStdGen
          let cc = (controlLoop (control gen))
          prettyPrint (maximum (blackboards cc))
