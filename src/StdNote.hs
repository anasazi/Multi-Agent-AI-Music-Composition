module StdNote 
( StdNote
) where

import NoteADT
import Test.QuickCheck
import Control.Monad
import qualified Data.List as L

data StdNote = N NoteLetter [Accidental] Octave deriving (Eq, Show)

instance Arbitrary StdNote where
  arbitrary = liftM3 N arbitrary arbitrary arbitrary

instance Note StdNote where
  midC = N C [] 4

  nl (N x _ _) = x
  ac (N _ x _) = x
  ot (N _ _ x) = x

  eqN a b = nl a == nl b && L.sort (ac a) == L.sort (ac b) && ot a == ot b
  ordN a b = if ot a /= ot b
              then compare (ot a) (ot b)
              else if nl a /= nl b
                    then compare (nl a) (nl b)
                    else let f a | a == Flat = (-1) | a == Sharp = 1 in
                          compare (sum . map f . ac $ a) (sum . map f . ac $ b)
                    --compare (L.sort (ac a)) (L.sort (ac b))

  eqL a b = nl a == nl b && ot a == ot b
  ordL a b = if ot a /= ot b
              then compare (ot a) (ot b)
              else compare (nl a) (nl b)

  -- first move a and b to the same staff line (letter and octave)
  -- then normalize accidentals, if notationally equal then enharmonically equal
  eqE a b
    | a `ordL` b == GT = b `eqE` a
    | a `ordL` b == EQ = normA a `eqN` normA b
    | otherwise        = nlup a `eqE` b

  ordE a b
    | a `ordL` b == GT = a `ordE` nlup b
    | a `ordL` b == EQ = let f a | a == Flat = (-1) | a == Sharp = 1 in
                          compare (sum . map f . ac $ a) (sum . map f . ac $ b)
    | otherwise        = nlup a `ordE` b

  sharp n = N (nl n) (Sharp : ac n) (ot n)
  flat n = N (nl n) (Flat : ac n) (ot n)
  otup n = N (nl n) (ac n) (1 + ot n)
  otdn n = N (nl n) (ac n) (ot n - 1)

  nlup n
    | nl n == B = N C (Flat : ac n) (1 + ot n)
    | nl n == E = N F (Flat : ac n) (ot n)
    | nl n == G = N A (Flat : Flat : ac n) (ot n)
    | otherwise = N (succ (nl n)) (Flat : Flat : ac n) (ot n)

  nldn n
    | nl n == C = N B (Sharp : ac n) (ot n - 1)
    | nl n == F = N E (Sharp : ac n) (ot n)
    | nl n == A = N G (Sharp : Sharp : ac n) (ot n)
    | otherwise = N (pred (nl n)) (Sharp : Sharp : ac n) (ot n)

  normA n = f n
    where 
    f n = N (nl n) (g (ac n)) (ot n)
    g = L.unfoldr h . sum . concat . (map . map $ j) . L.group . L.sort
    h i | i == 0 = Nothing | i < 0 = Just (Flat, i+1) | i > 0 = Just (Sharp, i-1)
    j a | a == Flat = (-1) | a == Sharp = 1

test = do
  quickCheck (prop_sharpApplied :: StdNote -> Bool)
  quickCheck (prop_flatApplied :: StdNote -> Bool)
  quickCheck (prop_otupApplied :: StdNote -> Bool)
  quickCheck (prop_otdnApplied :: StdNote -> Bool)
  quickCheck (prop_otUpDown_Inverse :: StdNote -> Bool)
  quickCheck (prop_otDownUp_Inverse :: StdNote -> Bool)
  quickCheck (prop_eqN_Reverse :: StdNote -> Bool)
  quickCheck (prop_eqN_Implies_ordN :: StdNote -> Bool)
  quickCheck (prop_ordN_Implies_eqN :: StdNote -> StdNote -> Property)
  quickCheck (prop_ordN_Holds :: StdNote -> StdNote -> Bool)
  quickCheck (prop_nlUpDown_Inverse :: StdNote -> Bool)
  quickCheck (prop_nlDownUp_Inverse :: StdNote -> Bool)
  quickCheck (prop_normA_Imdepotent :: StdNote -> Bool)
  quickCheck (prop_sharp_flat_Inverse :: StdNote -> Bool)
  quickCheck (prop_flat_sharp_Inverse :: StdNote -> Bool)
  quickCheck (prop_eqE_Reverse :: StdNote -> Bool)
  quickCheck (prop_eqN_Implies_eqE :: StdNote -> Property)
  quickCheck (prop_normA_eqE_Identity :: StdNote -> Bool)
  quickCheck (prop_sharp_flat_eqE_Identity :: StdNote -> Bool)
  quickCheck (prop_flat_sharp_eqE_Identity :: StdNote -> Bool)
  quickCheck (prop_ordE_Holds :: StdNote -> StdNote -> Bool)
  quickCheck (prop_eqL_Reverse :: StdNote -> Bool)
  quickCheck (prop_eqN_Implies_eqL :: StdNote -> Property)
  quickCheck (prop_ordL_Holds :: StdNote -> StdNote -> Bool)
