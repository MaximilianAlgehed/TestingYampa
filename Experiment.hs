import FRP.Yampa
import Test.QuickCheck

-- TODO: Implement something
--       related to control systems?

sin' :: SF Double Double
sin' = arrPrim sin

signal :: SF a Double
signal = time >>> sin'

type LoopTransferFunction = SF Double Double

fork :: SF a (a, a)
fork = arr id &&& arr id

main = do
  sequence_ $ map print $ take 10 $ embed signal (deltaEncode 0.1 [1, 1 ..])

-- type Property a = a -> Bool
type FRProp a = SF a  Bool
type FRGen  a = Gen (SF () a)

{- ARROW LAWS
 - arr id = id
 - arr (f >>> g) = arr f >>> arr g
 - first (arr f) = arr (first f)
 - first (f >>> g) = first f >>> first g
 - first f >>> arr fst = arr fst >>> f
 - first f >>> arr (id *** g) = arr (id *** g) >>> first f
 - first (first f) >>> arr assoc = arr assoc >>> first f
 -}
