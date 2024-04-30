module Library where
import PdePreludat
import GHC.Integer (Integer)

doble :: Number -> Number
doble numero = numero + numero

esMultiploDeTres :: Number -> Bool
esMultiploDeTres nro = mod nro 3 == 0

esMultiploDe :: (Number,Number) -> Bool
esMultiploDe (nro1, nro2) = mod nro2 nro1 == 0

esMultiploDe' :: Number -> Number -> Bool
esMultiploDe' nro1 nro2 = mod nro2 nro1 == 0

cubo :: Number -> Number
cubo nro = nro ^ 3

cuadruple :: Number -> Number
--cuadruple nro1 = (doble.doble) nro1
cuadruple = doble . doble   -- Simplificacion

main :: IO ()
main = do
    --print $ esMultiplodeTres 9
    --print $ esMultiplodeTres 10
    --print $ esMultiploDe (3,12)
    --print $ esMultiploDe (3,11)
    print $ cuadruple 3

