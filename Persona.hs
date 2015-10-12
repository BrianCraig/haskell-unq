module Persona(Persona) where

import TP1

type Nombre = String
type Edad = Int

data Persona = P Nombre Edad deriving (Show)

nombre :: Persona -> String
nombre (P n _) = n

edad :: Persona -> Int
edad (P _ e) = e 

crecer :: Persona -> Persona
crecer p = P (nombre p) ((edad p)+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre n p = P n (edad p)

esMenorQueLaOtra :: Persona -> Persona -> Bool
esMenorQueLaOtra p1 p2 = (edad p1) < (edad p2)

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA e (x:xs) = if (edad x) > e
                      then [x] ++ mayoresA e xs
                      else mayoresA e xs

edades :: [Persona] -> [Int]
edades [] = []
edades (x:xs) = ((edad x):(edades xs))

promedioEdad :: [Persona] -> Int
promedioEdad l = TP1.promedio (edades l)

elMasViejo :: [Persona] -> Persona
elMasViejo (x:[]) = x
elMasViejo (x:xs) = 
  let mv = elMasViejo xs
  in  if (esMenorQueLaOtra x mv) 
        then mv
        else x 


-- Datos para tests

p1 = (P "juan" 21)
p2 = (P "gabriel" 24)
p3 = (P "rocco" 18)
p4 = (P "maria" 23)
lp = [p1,p2,p3,p4]
