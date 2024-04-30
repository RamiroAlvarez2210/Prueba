-- CLASE 4/9

siguiente :: Num a => a -> a
siguiente nro = nro + 1

doble :: Integer -> Integer
doble nro = nro * 2

calcular :: Integer -> Integer
calcular nro | even nro = siguiente nro
             | otherwise = doble nro

calcular' :: (Integer, Integer) -> (Integer, Integer)
calcular' (nro, otroNum) = (primeroPar nro, segundoImpar otroNum)

primeroPar :: Integer -> Integer
primeroPar nro | even nro = doble nro
               | otherwise = nro

segundoImpar :: Integer -> Integer
segundoImpar nro | odd nro = siguiente nro
                 | otherwise = nro

and' :: Bool -> Bool -> Bool
and' valor otroValor | valor = otroValor
                     | otherwise = False

and'' :: Bool -> Bool -> Bool
and'' True otroValor = otroValor
and'' _ _ = False                     

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

or'' :: Bool -> Bool -> Bool
or'' unValor otroValor | unValor = True

type Nota = Integer
type Alumno = (String, Nota, Nota, Nota)

notaMaxima :: Alumno -> Nota
notaMaxima (_, nota, nota2, nota3) = nota `max` (nota2 `max` nota3) -- Notacion Infija

notaMaxima' :: Alumno -> Nota
notaMaxima' (_, nota, nota2, nota3) = max nota (max nota2 nota3)    -- Notacion Prefija

cuadruple :: Integer -> Integer
cuadruple nro = doble (doble nro)

esMayorA :: Integer -> Bool
esMayorA nro = doble(siguiente (nro + 2)) > 10 

-- CLASE 4/17

suma :: (Integer, Integer) -> Integer
suma (nro, otroNum) = nro + otroNum

suma' :: Integer -> Integer -> Integer
suma' nro otroNum = nro + otroNum

sigSuma8Impar :: Integer -> Bool
sigSuma8Impar nro = (odd . siguiente . suma' 8) nro

distancia :: (Double, Double) -> Double
distancia (x, y) = sqrt (x^2 + y^2)

data Punto= Plano{x :: Double, y :: Double} |
            Espacio{x :: Double, y :: Double, z :: Double}

distanciaAlOrigen :: Punto -> Double
distanciaAlOrigen (Plano coorX coorY) = sqrt (coorX^2 + coorY^2)
distanciaAlOrigen (Espacio coorX coorY coorZ) = sqrt (coorX^2 + coorY^2 + coorZ^2)

-- Interesante Uso de Propiedades

data Figura = Rectangulo{base :: Double, altura :: Double} |
              Circulo{radio :: Double}

area :: Figura -> Double
area (Rectangulo b a) = b * a
area (Circulo r) = pi * r

circulo :: Figura
circulo = Circulo 5

rectangulo :: Figura
rectangulo = Rectangulo 2 8

-- 

data Persona = Persona{nombre :: String, edad :: Int} deriving (Show)

julia :: Persona
julia = Persona "Julia" 20

cumplirAños :: Persona -> Persona
cumplirAños persona = persona{edad = edad persona + 1} 
--cumplirAños (Persona nom edad) = Persona nom (edad + 1) -- patern matching

-- Ejercicios
-- Punto 1

esNotaBochazo :: Int -> Bool
esNotaBochazo nota = nota < 6

aprobo :: (Int, Int) -> Bool
aprobo (nota1, nota2) = (not . esNotaBochazo)nota1 && 
                      (not . esNotaBochazo)nota2

promociono :: (Int, Int) -> Bool
promociono (nota1, nota2) = nota1 >= 8 && nota2 >=8

-- Para hacer una consulta: (not . esNotaBochazo . fst)(5,8)

--Punto 2

data Empleado = Comun{basico::Double, nombreEmp::Double} |
                Jefe{basico::Double, cantPers::Double, nombreJefe::String}

sueldo :: Empleado -> Double
sueldo (Comun basico _) = basico
sueldo (Jefe basico cantPer _) = basico + plus cantPer

plus :: Double -> Double
plus cant = cant * 500

--Punto 3

data Bebida = Cafe{nombreBebida::String} |
              Gaseosa{sabor::String, azucar::Int}

esEnergizante :: Bebida -> Bool
esEnergizante (Cafe nom) = nom == "Capuchino"
esEnergizante (Gaseosa "Pomelo" cantAzucar) = cantAzucar > 10
esEnergizante _ = False