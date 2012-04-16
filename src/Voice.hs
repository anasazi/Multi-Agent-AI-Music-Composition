module Voice
( Focus, Context, Voice
, focus, context, runV, makeVoice
, atStart, atEnd
, modify
, forward1, back1
, forward, back
, front, end
, getCurrentNote
, getForwardN, getBackN
, startTimeOfFocus, durationOfFocus, durationOfVoice
, goToTime
) where

import Note
import qualified Data.List as L
import Control.Monad
import Data.Maybe
import Control.Arrow

type Focus = [Note]
type Context = [Note]

newtype Voice = V (Focus,Context) deriving Show
focus (V (x,_)) = x
context (V (_,x)) = x
runV (V x) = x
makeVoice = V
instance Eq Voice where
  a == b = focus (front a) == focus (front b)

atStart, atEnd :: Focus -> Voice
atStart ns = V (ns,[])
atEnd   ns = V ([],reverse ns)

head' [] = Nothing
head' (x:_) = Just x

tail' [] = Nothing
tail' (_:xs) = Just xs

take' = L.genericTake
drop' = L.genericDrop
replicate' = L.genericReplicate
length' = L.genericLength

modify :: (Focus -> Focus) -> Voice -> Voice
modify f = runV >>> first f >>> makeVoice

forward1, back1 :: Voice -> Maybe Voice
forward1 v = do let (foc,cxt) = runV v
                focus' <- tail' foc
                x <- head' foc
                return (makeVoice (focus', x:cxt))
back1 v = do let (foc,cxt) = runV v
             context' <- tail' cxt
             x <- head' cxt
             return (makeVoice (x:foc, context'))

forward, back :: Integer -> Voice -> Maybe Voice
forward 0 v = return v
forward n v = do v' <- forward1 v
                 forward (n-1) v'
back 0 v = return v
back n v = do v' <- back1 v
              back (n-1) v'

front, end :: Voice -> Voice
front v = fromJust (back (length' (context v)) v)
end v = fromJust (forward (length' (focus v)) v)

getCurrentNote :: Voice -> Maybe Note
getCurrentNote = head' . focus

getForwardN, getBackN :: Integer -> Voice -> Maybe [Note]
getForwardN 0 v = Just []
getForwardN n v = do v' <- forward1 v
                     ns <- getForwardN (n-1) v'
                     cur <- getCurrentNote v
                     Just (cur : ns)
getBackN 0 v = Just []
getBackN n v = do v' <- back1 v
                  ns <- getBackN (n-1) v'
                  cur <- getCurrentNote v
                  Just (cur : ns)

startTimeOfFocus, durationOfFocus, durationOfVoice :: Voice -> Double                  
startTimeOfFocus = sum . map durAsNum . context
durationOfFocus = sum . map durAsNum . focus
durationOfVoice = durationOfFocus . front

goToTime :: Voice -> Double -> Maybe Voice
goToTime v t = goToTimeHelper t v (startTimeOfFocus v)
  where 
  curDur v = fromMaybe 0 (getCurrentNote v >>= Just . durAsNum)
  goToTimeHelper tarT v curST | curST <= tarT && tarT < curST + curDur v = Just v
                              | tarT < curST = do v' <- back1 v
                                                  cur' <- getCurrentNote v'
                                                  let dur = durAsNum cur'
                                                  goToTimeHelper tarT v' (curST - dur)
                              | tarT > curST = do v' <- forward1 v
                                                  cur' <- getCurrentNote v'
                                                  let dur = durAsNum cur'
                                                  goToTimeHelper tarT v' (curST + dur)
