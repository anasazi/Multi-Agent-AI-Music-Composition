module BlackboardSystem.KnowledgeSource
( KnowledgeSource(..)
, isGenerator, isHardRule
, makeSoftRule, makeHardRule, makeGenerator
) where

import BlackboardSystem.Blackboard

makeSoftRule op = KS { isTester = True, isSoftRule = True, operate = op }
makeHardRule op = KS { isTester = True, isSoftRule = False, operate = op }
makeGenerator op = KS { isTester = False, isSoftRule = False, operate = op }

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
