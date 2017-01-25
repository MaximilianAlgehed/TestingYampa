import FRP.Yampa

fork :: SF a (a, a)
fork = arr id &&& arr id

feedbackLoop :: LoopTransferFunction -> LoopTransferFunction
feedbackLoop ltf = loopPre 0 $ arr (uncurry (-)) >>> ltf >>> fork

pRegulator :: Double -> LoopTransferFunction
pRegulator p = arr (*p)

iRegulator :: Double -> LoopTransferFunction
iRegulator i = integral >>> arr (*i)

dRegulator :: Double -> LoopTransferFunction
dRegulator d = derivative >>> arr (*d)

pid :: Double -- p
    -> Double -- i
    -> Double -- d
    -> LoopTransferFunction
pid p i d = (arr id &&& fork) >>> (pRegulator p) *** (iRegulator i) *** (dRegulator d) >>> second (arr (uncurry (+))) >>> arr (uncurry (+))

firstOrderSystem :: Double -> Double -> LoopTransferFunction
firstOrderSystem k t = loopPre 0 $ (arr (*k) *** (derivative >>> arr (*t))) >>> arr (uncurry (-)) >>> fork
