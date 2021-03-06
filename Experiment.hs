{-# LANGUAGE TypeApplications, Arrows #-}
import FRP.Yampa
import Test.QuickCheck

-- I'm not liking this, it needs to be re-expressed somehow
type FRProp a = SF a Bool
type FRGen  a = Gen (SF () a)

-- Assert that a property only needs to be true before a time t
before :: Time -> FRProp a -> FRProp a 
before t sig =
  proc x ->
    do
      b    <- sig  -< x
      tnow <- time -< ()
      returnA -< if tnow < t then b else True

beforeOld t sig = (sig &&& time) >>> arr (\(b, tnow) -> if tnow < t then b else True)

prop_before = do
  t <- abs <$> arbitrary
  input <- arbPoisson 1 
  return $ (before t input) &&& (beforeOld t input) >>> arr (uncurry (==))

-- Assert that a property only needs to be true after a time t
after :: Time -> FRProp a -> FRProp a 
after t sig =
  proc x ->
    do
      b    <- sig  -< x
      tnow <- time -< ()
      returnA -< if tnow > t then b else True

afterOld t sig = (sig &&& time) >>> arr (\(b, tnow) -> if tnow > t then b else True)

prop_after t = do
  t <- abs <$> arbitrary
  input <- arbPoisson 1 
  return $ (Main.after t input) &&& (afterOld t input) >>> arr (uncurry (==))

-- | infinite poisson process
poisson :: Time -> Gen [Time]
poisson t = sequence $ repeat $ (\u -> (0 - (log u))/t) <$> choose (0, 1)

-- Poisson sample
occasional :: (Arbitrary a) => Time -> FRGen (Event a)
occasional t = do
  times <- poisson t 
  as    <- sequence $ repeat arbitrary
  return $ afterEach $ zip times as

-- Arbitrary values at poisson distributed times
arbPoisson :: (Arbitrary a) => Time -> FRGen a
arbPoisson t = do
  init   <- arbitrary
  events <- occasional t
  return $ events >>> hold init

-- Buggy prop
prop_abs_buggy :: FRProp Double
prop_abs_buggy = arr abs >>> arr (>0.01) -- == arr (ans . (> 0.01))

-- Sample with an end time
type SamplingStrategy = Double -> [Double]

-- Test a property by sampling at some predefined intervals
testAt :: (Arbitrary a) => FRProp a -> [Time] -> Gen Bool
testAt dut samples = do
  testCase <- arbPoisson 1
  return $ and $ embed (testCase >>> dut) ((), [(t, Nothing) | t <- samples]) 

{- ARROW LAWS, dervie FRProp and FRGen from these?
               maybe derive FRProp and FRGen from the category theoretical interpretation?
   arr id = id
   arr (f >>> g) = arr f >>> arr g
   first (arr f) = arr (first f)
   first (f >>> g) = first f >>> first g
   first f >>> arr fst = arr fst >>> f
   first f >>> arr (id *** g) = arr (id *** g) >>> first f
   first (first f) >>> arr assoc = arr assoc >>> first f
 -}
