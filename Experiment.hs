{-# LANGUAGE TypeApplications #-}
import FRP.Yampa
import Test.QuickCheck

signal :: SF a Double
signal = time >>> arr sin

fork :: SF a (a, a)
fork = arr id &&& arr id

main = do
  sequence_ $ map print $ take 63 $ embed signal (deltaEncode 0.1 [1, 1 ..])

-- type Property a = a -> Bool
type FRProp a = SF a  Bool
type FRGen  a = Gen (SF () a)

forall :: FRGen a -> FRProp a -> FRGen Bool
forall gen prop = (>>> prop) <$> gen

-- FIX THIS!
arb :: (Arbitrary a) => FRGen a
arb = do
  a <- arbitrary
  return $ constant a

prop_abs_idempotent :: FRGen Bool
prop_abs_idempotent =
  forall arb $ arr (abs @Double) >>> arr (>0)

{- ARROW LAWS, dervie FRProp and FRGen from these?
 -             maybe derive FRProp and FRGen from the category theoretical interpretation?
 - arr id = id
 - arr (f >>> g) = arr f >>> arr g
 - first (arr f) = arr (first f)
 - first (f >>> g) = first f >>> first g
 - first f >>> arr fst = arr fst >>> f
 - first f >>> arr (id *** g) = arr (id *** g) >>> first f
 - first (first f) >>> arr assoc = arr assoc >>> first f
 -}
