----- inicio datas
data Nacion = UnaNacion{
capital :: String,
	poblacion :: Int,
	ejercito :: [Soldado]}
	deriving Show
	
data Soldado = UnSoldado{
tipo :: String,
	fuerza :: Int,
	armadura :: Bool}
	deriving Show
	
----- fin datas

-- constantes
sold = UnSoldado{tipo = "espadachin",fuerza=150,armadura = True}
explorador = UnSoldado{tipo = "espadachin",fuerza=170,armadura = False}
arq = UnSoldado{tipo = "arq",fuerza=100,armadura = False}
ejercitoBlanco = [sold, sold, sold, explorador, explorador, arq, arq, sold, arq]
gondor = UnaNacion{capital = "Gondor",poblacion=500,ejercito = ejercitoBlanco}

orco = UnSoldado{tipo = "lanzero", fuerza=25, armadura=True}
ejercitoRojo = replicate 100 orco

-- Funciones

faramir :: [Soldado] -> [Soldado]
faramir listaSoldados = map restarFuerza listaSoldados

gandalf :: [Soldado] -> [Soldado]
gandalf listaSoldados = map sumarFuerza listaSoldados

pippin :: [Soldado] -> [Soldado]
pippin listaSoldados = filter tieneArmadura listaSoldados


restarFuerza soldado = soldado{fuerza = (fuerza soldado) -2}
sumarFuerza soldado = soldado{fuerza = (fuerza soldado)+200}
tieneArmadura soldado = armadura soldado 

prepararEjercito unaNacion heroe =  heroe (ejercito unaNacion) 


--batalla unaNacion unEjercito heroe | sum (map fuerza ejercito) > sum (map fuerza (prepararEjercito nacion heroe))  = unaNacion { capital= capital unaNacion++"CONQUISTADA" , poblacion= max 0 ((poblacion unaNacion) - 1000, ejercito=[]}
--  | sum (map fuerza ejercito) == sum (map fuerza (prepararEjercito nacion heroe))  = unaNacion { capital= capital unaNacion++" Sobrevivo" , poblacion= (poblacion unaNacion) div 2, ejercito= drop 2 (ejercito unaNacion)}
--  | otherwise = unaNacion { capital= capital unaNacion++" Sobrevivo" , poblacion= (poblacion unaNacion) div 2 }

batalla :: Nacion -> [Soldado] -> ([Soldado] -> [Soldado]) -> String
batalla nacion ejercito heroe | sum (map fuerza ejercito) > sum (map fuerza (prepararEjercito nacion heroe)) = "CONQUISTADA"
                    | sum (map fuerza ejercito) == sum (map fuerza (prepararEjercito nacion heroe)) = "EMPATE"
					| otherwise = "GANA DEFENSOR"

batallaMejorada :: Nacion -> [Soldado] -> ([Soldado] -> [Soldado]) -> String
batallaMejorada nacion ejercito heroe | fuerzaTotal ejercito sum > fuerzaTotal (prepararEjercito nacion heroe) poderDeLosBuenos = "CONQUISTADA" 
 | fuerzaTotal ejercito sum == fuerzaTotal (prepararEjercito nacion heroe) poderDeLosBuenos = "EMPATE" 
 | otherwise = "GANA DEFENSOR"

fuerzaTotal ejercito f = f (map fuerza ejercito)
poderDeLosBuenos lista = sum (map (2*) lista)
valorEjercito ejercito funcion = funcion (map fuerza ejercito)









