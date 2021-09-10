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

-- 4. cruzarPollos (sin usar expresion lambda porque aun no lo vimos)
cruzar (UnPollo nombre1 diasVivos1 peso1 artes1) (UnPollo nombre2 diasVivos2 peso2 artes2)
    = UnPollo (nombre1 ++ nombre2) (diasVivos1 + diasVivos2) (peso1 + peso2) (artes1 ++ artes2)

cruzarPollos [unPollo] = unPollo
cruzarPollos (pollo:(otroPollo:restoDePollos)) 
    = cruzarPollos ((cruzar pollo otroPollo):restoDePollos)


-- Segunda Parte: Pollos Ninjas

pau = UnPollo "Paula" 2 300 ["judo"]

-- entrenadores


--a. arguiniano
arguiniano pollo = engordar pollo 100

--b miyagi
agregarSiNoEsta elemento lista
    | elem elemento lista = lista -- si esta devuelvo la lista
    | otherwise = elemento:lista -- sino devuelvo una lista con el elemento delante.

miyagi pollo = pollo {artesMarciales = agregarSiNoEsta "karate" (artesMarciales pollo)}

--c marcelito
marcelito pollo = pollo {artesMarciales = []}

--d brujaTapita
data Raton = UnRaton { pesoRaton :: Number, -- lo llamo pesoRaton para que no tenga conflicto con el peso del pollo
                       altura :: Number, 
                       cantidadBigotes :: Number}
    deriving Show
alimentoQueProvee (UnRaton peso altura bigotes) = peso * altura - bigotes

brujaTapita raton pollo = engordar pollo (alimentoQueProvee raton)

--e marioBros, aplicando 2 veces agregarSiNoEsta, para agregar "saltar" y el arte
-- pasado a la funcion.
marioBros pollo arteMarcial 
    = pollo { nombre = "Super mario " ++ (nombre pollo),
              artesMarciales = agregarSiNoEsta arteMarcial (agregarSiNoEsta "saltar" (artesMarciales pollo)) }

-- Tambien podemos hacer una funcion auxiliar que agregue una lista de elementos
-- a otra lista si estos no estan presentes.
agregarVariosSiNoEstan [] lista = lista
agregarVariosSiNoEstan (e:es) lista = agregarVariosSiNoEstan es (agregarSiNoEsta e lista)

-- marioBros2, usando agregarVariosSiNoEstan
marioBros2 pollo arteMarcial 
    = pollo { nombre = "Super mario " ++ (nombre pollo),
              artesMarciales = agregarVariosSiNoEstan ["saltar", arteMarcial] (artesMarciales pollo)}

-- una tercera opcion, usando @ para poder hacer pattern matching, pero
-- tener el valor como un todo asignado a una variable, p en este caso.
-- De esta forma nos ahorramos usar las funciones para acceder cada campo.
marioBros3 p@(UnPollo nombre _ _ artes) arteMarcial
    = p {nombre = "Super mario " ++ nombre,
         artesMarciales = agregarVariosSiNoEstan ["saltar", arteMarcial] artes}


-- para los puntos siguientes se requieren temas que no vimos.

