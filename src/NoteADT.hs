module NoteADT where

class Note a where
	midC	:: a			-- The representation of Middle C (C4)
	sharp	:: a -> a	-- Apply a sharp
	flat	:: a -> a	-- Apply a flat
	normA	:: a -> a	-- Simplify the sharps and flats while keeping the plain note fixed.
	normN :: a -> a	-- Simplify the note to have at most one accidental.

prop_normA_Idempotent n = (normA n) == (normA . normA $ n)
prop_normN_Idempotent n = (normN n) == (normN . normN $ n)
prop_normA_sharp_flat_Inverse n = normA n == (normA . sharp . flat . normA $ n)
prop_normA_flat_sharp_Inverse n = normA n == (normA . flat . sharp . normA $ n)
prop_normA_normN_Associative n = (normA . normN $ n) == (normN . normA $ n) 
