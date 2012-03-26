module BlackboardSystem.KnowledgeSource
( KnowledgeSource
, isTester, isSoftRule, operate
, isGenerator, isHardRule
, makeSoftRule, makeHardRule, makeGenerator
) where

import BlackboardSystem.Blackboard

makeSoftRule op desc = KS { isTester = True, isSoftRule = True, operate = op, description = desc }
makeHardRule op desc = KS { isTester = True, isSoftRule = False, operate = op, description = desc }
makeGenerator op desc = KS { isTester = False, isSoftRule = False, operate = op, description = desc }

data KnowledgeSource = KS { isTester :: Bool
                          , isSoftRule :: Bool
                          , operate :: Blackboard -> Blackboard
                          , description :: String
                          }

instance Show KnowledgeSource where
  show ks = kstype ++ description ks
    where kstype | isGenerator ks = "generator: "
                 | isSoftRule ks = "soft tester: "
                 | isHardRule ks = "hard tester: "

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
