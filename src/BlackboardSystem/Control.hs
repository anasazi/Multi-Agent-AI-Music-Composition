module BlackboardSystem.Control
(
) where

{-
The control knows a list of test agents and a list of generator agents.
It keeps a sorted list of blackboard (longest with least errors on top).
It also keeps a map from blackboards to test results and needed tests.
If the top element has passed all the tests, it send it to a random generator.
It compares the old blackboard to the new and identifies what elements changed and need new tests.
If the top element needs a test done, it sends it to that agent.
Once the agent returns, it reads the test result from the blackboard and stores it.
Blackboards returned from agents are placed in the sorted list properly.
If the top element is long enough to meet the goal length, it returns that element and stops.
-}
