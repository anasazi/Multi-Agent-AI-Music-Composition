import qualified FirstSpecies
import qualified GeneralSpecies

import BlackboardSystem.Control
import BlackboardSystem.KnowledgeSource

import Music.Note
import Music.Scale
import Util.Zipper

import System.Random

cantusfirmus = enterFront [ midC, up . up . up . sharp . sharp . sharp . sharp . sharp $ midC ] --, (sharp (sharp (up midC))) ]

allAgents = GeneralSpecies.agents ++ FirstSpecies.agents

control gen = makeControl allAgents major cantusfirmus gen

main = do 
          gen <- getStdGen
          --let gen = mkStdGen 1
          let cc = (controlLoop (control gen))
          print $ targetDuration cc
          mapM_ print (blackboards cc)
