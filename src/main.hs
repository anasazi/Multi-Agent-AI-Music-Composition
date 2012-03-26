import FirstSpecies
import BlackboardSystem.Control
import BlackboardSystem.KnowledgeSource

import Music.Note
import Music.Scale
import Util.Zipper

import System.Random

cantusfirmus = enterFront [ midC, (sharp (up midC)) ]

control gen = makeControl [ firstSpeciesGenerator ] major cantusfirmus gen

main = do 
          gen <- getStdGen
          --let gen = mkStdGen 1
          print (controlLoop (control gen))
