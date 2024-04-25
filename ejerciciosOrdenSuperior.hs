data Bebida = Cafe {nombreBebida :: String} |
            Gaseosa {sabor ::String , azucar :: Integer}

esEnergizante :: Bebida -> Bool
esEnergizante (Cafe "capuchino") =  True
esEnergizante (Gaseosa "pomelo" cantAzucar) = cantAzucar > 10
esEnergizante _  = False


data Persona = Persona {nombre :: String, edad :: Int} deriving Show

julia :: Persona
julia = Persona "julia" 21

pedro :: Persona
pedro = Persona "pedro" 26

cumplirAños :: Persona -> Persona
cumplirAños persona = persona {edad = edad persona + 1}


find' :: (a -> Bool) -> [a] -> a
find' condicion lista = (head .filter condicion) lista

data Politico
     = Politico {proyectosPresentados :: [String], sueldo :: Integer,  edadPolitico :: Int } deriving Show 
politicos = [ Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81, Politico ["tratar de reconquistar luchas sociales"] 10000 63, Politico ["tolerancia 100 para delitos"] 15500 49 ]


{- a 
ghci> find'  ((<50).edadPolitico) politicos
Politico {proyectosPresentados = ["tolerancia 100 para delitos"], sueldo = 15500, edadPolitico = 49} -}


{- b
ghci> find' ((>3).length. proyectosPresentados) politicos
Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 19182"], sueldo = 20000, edadPolitico = 81}  
-}


{- c
ghci> find' (any ((>3).length.words).proyectosPresentados) politicos 
Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 19182"], sueldo = 20000, edadPolitico = 81}  
-}

type Nombre = String
type Notas = [Int]
data Alumno = Alumno {nombreAlumno :: Nombre, notas :: Notas}

promediosAlumnos :: [Alumno] -> [(Nombre, Int)]
promediosAlumnos alumnos = map (\alumno -> (nombreAlumno alumno, (promedio.notas)alumno))  alumnos

promedio :: Notas -> Int
promedio notas = (sum notas) `div` (length notas)



