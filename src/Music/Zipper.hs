module Zipper
( enterFront, enterBack
, forward, back
, forward', back'
, front, end
, modify, focus, context, escape
, Zipper
) where

import Control.Arrow

-- A zipper for lists

data Zipper a = Z { foc :: [a], cxt :: [a] } deriving (Eq, Show)

enterFront, enterBack :: [a] -> Zipper a
enterFront ls = Z ls []
enterBack  ls = Z [] (reverse ls)

-- safe
head' [] = Nothing
head' (x:_) = Just x
tail' [] = Nothing
tail' (_:x) = Just x

forward, back :: Zipper a -> Maybe (Zipper a)
forward z = do foc' <- tail' . foc $ z
               x <- head' . foc $ z
               return z { foc = foc', cxt = x : (cxt z) }
back z = do x <- head' . cxt $ z
            cxt' <- tail' . cxt $ z
            return z { foc = x : (foc z), cxt = cxt' }

-- unsafe
forward', back' :: Zipper a -> Zipper a
forward' z = z { foc = tail (foc z), cxt = head (foc z) : (cxt z) }
back' z = z { foc = head (cxt z) : (foc z), cxt = tail (cxt z) }

front, end :: Zipper a -> Zipper a
front z | null (cxt z)  = z
        | otherwise     = front z { foc = head (cxt z) : (foc z), cxt = tail (cxt z) }
end z | null (foc z) = z
      | otherwise    = end z { foc = tail (foc z), cxt = head (foc z) : (cxt z) }

modify :: Zipper a -> ([a] -> [a]) -> Zipper a
modify z f = z { foc = f (foc z) }

focus = foc
context = cxt
escape = foc . front
