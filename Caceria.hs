
import Text.Show.Functions

data Personaje = Personaje {experiencia :: Float, fuerza :: Float, elemento :: Elemento} deriving Show 

type Elemento = Float -> Float

nivel (Personaje experiencia _ _) = ceiling(experiencia^2/(experiencia+1))

capacidad :: Personaje -> Float
capacidad (Personaje _ fuerza elemento) = elemento fuerza

espadaOxidada = (1.2*)
katanaFilosa = (10+).(0.9*)
sableLambdico cm = ((1+cm/100)*)
redParadigmatica = sqrt
baculoDuplicador x= x* 2
espadaMaldita = espadaOxidada.sableLambdico 89

julia = Personaje 40 30 redParadigmatica
pepe = Personaje 50 10 espadaOxidada

pedro = Personaje 70 20 (sableLambdico 10)

type Alquimista = Personaje -> Personaje

aprendiz :: Alquimista
aprendiz personaje = alterarElemento (2*) personaje

alterarElemento :: Elemento -> Alquimista
alterarElemento f personaje = personaje {elemento = f.elemento personaje}


maestroAlquimista :: Int -> Alquimista
maestroAlquimista años personaje = alterarElemento (extraPorAnti años). aprendiz $ personaje


extraPorAnti 0 = id
extraPorAnti años = (*1.1).extraPorAnti (años -1)


estafador :: Alquimista
estafador personaje = personaje {elemento = id}


nuevoAlquimista :: Alquimista
nuevoAlquimista personaje = personaje {elemento = (\nro -> nro + (fuerza personaje))}


alquimistasPositivos :: Float -> Personaje -> [Alquimista] -> [Alquimista] 
alquimistasPositivos valor personaje alquimistas = filter (superaValor valor personaje) alquimistas

superaValor :: Float -> Personaje -> Alquimista -> Bool
superaValor valor personaje alquimista = ((>valor).capacidad.alquimista) personaje

convieneATodos :: Personaje -> [Alquimista] -> Bool
convieneATodos personaje alquimistas = all (superaValor (capacidad personaje) personaje) alquimistas


{-
convieneATodos julia [aprendiz, nuevoAlquimista]
True
-}

data Monstruo = Monstruo {especie :: String, resistencia :: Float, habilidades :: [Habilidad]} deriving Show


type Habilidad = (String, String)


esAgresivo :: Monstruo -> Bool
esAgresivo monstruo = (tieneMayoriaHabilidadesOfensivas.habilidades) monstruo && ((>0).resistencia) monstruo && (not.especieInofensiva.especie $ monstruo)


tieneMayoriaHabilidadesOfensivas :: [Habilidad] ->Bool
tieneMayoriaHabilidadesOfensivas habilidades = (length.filter(esOfensiva.tipo)) habilidades > div (length habilidades) 2


esOfensiva :: String -> Bool
esOfensiva "magica" = True
esOfensiva "fisica" = True
esOfensiva _ = False

tipo = snd


especieInofensiva :: String -> Bool
especieInofensiva especie = elem especie ["animal", "chocobo"]


-- A la Caza
leGana :: Personaje -> Monstruo -> Bool
leGana personaje monstruo = capacidad personaje > resistencia monstruo

pelearConTodos :: Personaje ->[Monstruo] -> Personaje
pelearConTodos personaje monstruos = foldl pelear  personaje monstruos


pelear :: Personaje -> Monstruo -> Personaje
pelear personaje monstruo | leGana personaje monstruo = alterarExperiencia 100 personaje
                          | otherwise = alterarElemento (*0.9).alterarExperiencia (-50) $ personaje



alterarExperiencia :: Float -> Personaje -> Personaje
alterarExperiencia puntos personaje =  personaje { experiencia = experiencia personaje + puntos}

noPuedeVencer :: [Monstruo] -> Alquimista -> Personaje -> Bool
noPuedeVencer monstruos alquimista personaje =  any (not.leGana (alquimista personaje)) monstruos