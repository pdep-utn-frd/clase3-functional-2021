{-
Consigna:
https://docs.google.com/document/d/1jikh4Y-Bl_2CRSy6ROV6P9RJlZva-TJBJ3iHSVXrNus
-}
module PollosNinjas where
import PdePreludat

data Pollo = UnPollo {
    nombre :: String,
    diasVivos :: Number,
    peso :: Number,
    artesMarciales :: [String]
} deriving Show

gin = UnPollo "ginger" 10 150 []
roc = UnPollo "rocky" 1 3000 []

--1 engordar
engordar pollo gramos = pollo { peso = (peso pollo) + gramos }

-- 2. esMayorDeEdad
esMayorDeEdad pollo = diasVivos pollo > 30*6

-- 3. sinArtesMarciales
sinArtesMarciales pollo = null (artesMarciales pollo)
-- una alternativa usando pattern matching
-- ultimoCampoVacio (UnPollo _ _ _ []) = True
-- ultimoCampoVacio _ = False

-- 4. Pendiente


-- Segunda Parte: Pollos Ninjas

pau = UnPollo "Paula" 2 300 ["judo"]

-- entrenadores


