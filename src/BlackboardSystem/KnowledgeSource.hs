module BlackboardSystem.KnowledgeSource
( KnowledgeSource(..)
, isGenerator, isHardRule
) where

import BlackboardSystem.Blackboard

data KnowledgeSource = KS { isTester :: Bool
                          , isSoftRule :: Bool
                          , operate :: Blackboard -> Blackboard
                          }

isGenerator = not . isTester
isHardRule ks = isTester ks && not (isSoftRule ks)

{-
class KnowledgeSource ks where
  isTester :: ks -> Bool
  isGenerator :: ks -> Bool

  isTester = not . isGenerator
  isGenerator = not . isTester

  isHardRule :: ks -> Bool
  isSoftRule :: ks -> Bool

  isHardRule = uncurry (&&) <<< isTester &&& not . isSoftRule
  isSoftRule = uncurry (&&) <<< isTester &&& not . isHardRule

  operate :: Blackboard -> ks -> Blackboard
  -}
