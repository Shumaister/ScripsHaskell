import System.Random

-- Listas de variables

data Empleado = Empleado
 { numeroInterno :: Int,
  nombre :: String,
  apellido :: String,
  sector :: String,
  probabilidad :: Int} deriving (Show)

factory = [Empleado 1 "Gaby" "M" "Hibrid" 3,Empleado 2 "Mile" "Farotto" "Desa" 3,Empleado 3 "Yani" "Dias" "Test" 3,Empleado 4 "Juanma" "Vallejos" "Desa" 3,Empleado 5 "Mati" "DeLaIglesia" "PM" 5,Empleado 1 "Rodri" "M" "Hibrid" 3,Empleado 1 "Gaby" "M" "Hibrid" 3,Empleado 1 "Gaby" "M" "Hibrid" 3,Empleado 1 "Gaby" "M" "Hibrid" 3,Empleado 1 "Ana" "M" "Hibrid" 3,Empleado 1 "Pablo" "M" "Hibrid" 3,Empleado 1 "Fabi" "M" "Hibrid" 3,Empleado 1 "Maxi" "M" "Hibrid" 3]


sacarPerdedor indice_perdedor lista  = take (indice_perdedor-1) lista ++ drop indice_perdedor lista

tomarAlgunos  =  soloNombre.(sacarPerdedor 2)

soloNombre lista = map nombre lista

elElegido :: [Double] -> Int
elElegido listaNumero =  ceiling (10*(listaNumero !! 0))

ganador g = (factory !! (elElegido (take 1 (randoms g :: [Double]))))

main = do
  g <- getStdGen
  print $ ganador g