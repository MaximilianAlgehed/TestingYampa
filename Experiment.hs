{-# LANGUAGE TypeApplications #-}
import FRP.Yampa
import Test.QuickCheck

-- type Property a = a -> Bool
type FRProp a = SF a  Bool
type FRGen  a = Gen (SF () a)

forall :: FRGen a -> FRProp a -> FRGen Bool
forall gen prop = (>>> prop) <$> gen

poisson :: Time -> Gen [Time]
poisson t = sequence $ repeat $ (\u -> (0 - (log u))/t) <$> choose (0, 1)

-- Poisson sample
occasional :: (Arbitrary a) => Time -> FRGen (Event a)
occasional t = do
  times <- poisson t 
  as    <- sequence $ repeat arbitrary
  return $ afterEach $ zip times as

-- FIX THIS!
arb :: (Arbitrary a) => Time -> FRGen a
arb t = do
  init   <- arbitrary
  events <- occasional t
  return $ events >>> hold init

prop_abs_idempotent :: FRGen Bool
prop_abs_idempotent =
  forall (arb 1) $ arr (abs @Double) >>> arr (>0.01)

-- Sample with an end time
type SamplingStrategy = Double -> [Double]

testAt :: FRGen Bool -> [Time] -> IO Bool
testAt gen samples = do
  testCase <- generate gen
  return $ and $ embed testCase ((), [(t, Nothing) | t <- samples]) 

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
