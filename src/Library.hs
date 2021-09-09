{- PDP 2021 - 9/9/2021
Temas:
- Recursividad
-}
module Library where
import PdePreludat

factorial 0 = 1 -- caso base 
factorial n = n * factorial (n - 1) -- caso recursivo

sumatoria [] = 0
sumatoria (primerElemento:resto) = primerElemento + sumatoria resto

-- sumatoria [1,2,3]
-- = 1 + sumatoria [2, 3]
-- = 1 + (2 + sumatoria [3])
-- = 1 + (2 + (3 + sumatoria []))
-- = 1 + (2 + (3 + 0))

data Alumno = UnAlumno {legajo :: String}

tomarParcial (UnAlumno legajo) = undefined

-- una lista es o bien:
-- * una lista vacia,
-- * un elemento concatenado a otra lista

duplicarElementos [] = []
duplicarElementos (primerElem:resto) = 2 * primerElem : duplicarElementos resto

-- duplicarElementos [1,2,3]
-- = 2*1 : duplicarElementos [2,3]
-- = 2*1 : (2 * 2 : duplicarElementos [3])
-- = 2*1 : (2 * 2 : (2 * 3 : duplicarElementos []))
-- = 2*1 : (2 * 2 : (2 * 3 : []))
-- = 2:(4:(6:[]))
-- = [2,4,6]

-- En haskell existe un tipo de dato similar que se llama Maybe
data QuizaUn t = Nada | Un t deriving Show

-- En Java hay un tipo de datos similar llamado Optional
-- En Scala/Rust Option
data PuedeHaberUnNumero = NoHayNada | HayUnNumero Number
    deriving Show

primerElementoSeguro [] = NoHayNada
primerElementoSeguro (x:_) = HayUnNumero x

primerElemento [] = error "lista vacia"
primerElemento (x:_) = x

ultimoElemento [] = error "lista vacia"
ultimoElemento [x] = x
ultimoElemento (_:cola) = ultimoElemento cola
