module Voice
(
) where

import Control.Arrow

import Note -- the main thing we'll be using

{- At its core, a musical voice is a sequence of notes.
If we assume that the notes don't overlap, then we can map R+ to the voice. This is useful for lookup.

We'll examine voices through a zipper, which enables easy movement. 
For efficiency, we probably want to keep the start time of a note instead of recomputing it each time.

-}

-- TODO change to use newtypes so that we'll be able to enforce read only on the cantus firmus.

-- first element of the list is the first note
type Voice = [ Note ]

type VoiceZipper = ( Voice, Voice ) -- the sub voice and the reversed prefix
 
--- basic movement
forward, back :: VoiceZipper -> Maybe VoiceZipper

forward ( [], _ ) = Nothing
forward ( (n:ns) , prefix ) = Just ( ns, n : prefix )

back ( _, [] ) = Nothing
back ( ns, (p:ps) ) = Just ( p : ns, ps )

--- move to edge
start, end :: VoiceZipper -> VoiceZipper

start vz@( _, [] ) = vz
start ( ns, (p:ps) ) = start ( p : ns, ps )

end vz@( [], _ ) = vz
end ( (n:ns), ps ) = end ( ns, n : ps )

-- change the voice
modify :: ( Voice -> Voice ) -> VoiceZipper -> VoiceZipper
modify = first 

-- shed the zipper
peek :: VoiceZipper -> Voice
peek = fst

-- TODO given a time, find the corresponding note
