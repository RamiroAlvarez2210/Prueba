module Library where
import PdePreludat
import GHC.Integer (Integer)

doble :: Number -> Number
doble numero = numero + numero

esMultiplodeTres :: Number -> Bool
esMultiplodeTres nro = mod nro 3 == 0
