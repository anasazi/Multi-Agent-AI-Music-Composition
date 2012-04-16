module Agent
( Agent
, isTester, isSoftRule, operate, description
, isGenerator, isHardRule
, makeSoftRule, makeHardRule, makeGenerator
) where

import Blackboard

data Agent = Agent
  { isTester :: Bool
  , isSoftRule :: Bool
  , operate :: Blackboard -> Blackboard
  , description :: String
  }

instance Show Agent where
  show a = agentType ++ description a
    where agentType | isGenerator a = "generator: "
                    | isSoftRule a = "soft tester: "
                    | isHardRule a = "hard tester: "

isGenerator = not . isTester
isHardRule a = isTester a && not (isSoftRule a)

makeSoftRule op desc = Agent { isTester = True, isSoftRule = True, operate = op, description = desc }
makeHardRule op desc = Agent { isTester = True, isSoftRule = False, operate = op, description = desc }
makeGenerator op desc = Agent { isTester = False, isSoftRule = False, operate = op, description = desc}
