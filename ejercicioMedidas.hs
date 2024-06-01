

type Bien = (String,Float)
data Ciudadano = UnCiudadano {profesion :: String, sueldo :: Float, 
cantidadDeHijos :: Float, bienes :: [Bien] } deriving Show

type Ciudad = [Ciudadano]

homero = UnCiudadano "SeguridadNuclear" 9000 3 [("casa",50000), ("deuda",-70000)]
frink = UnCiudadano "Profesor" 12000 1 []
krabappel = UnCiudadano "Profesor" 12000 0 [("casa",35000)]
burns = UnCiudadano "Empresario" 300000 1 [("empresa",1000000),("empresa",500000),("auto",200000)]

springfield = [homero, burns, frink, krabappel] 

diferenciaDePatrimonio :: Ciudad -> Float
diferenciaDePatrimonio ciudad = (patrimonio.ciudadanoSegun mayorPatrimonio) ciudad  - (patrimonio.ciudadanoSegun menorPatrimonio) ciudad

patrimonio :: Ciudadano -> Float
patrimonio ciudadano = foldl (\sem (_, bien)-> sem + bien) (sueldo ciudadano) (bienes ciudadano)

ciudadanoSegun :: (Ciudadano ->Ciudadano -> Ciudadano) -> Ciudad -> Ciudadano
ciudadanoSegun f ciudad = foldl1 f ciudad 

mayorPatrimonio:: Ciudadano -> Ciudadano -> Ciudadano
mayorPatrimonio unCiudadano otroCiudadano | patrimonio unCiudadano > patrimonio otroCiudadano = unCiudadano
                                          | otherwise = otroCiudadano

menorPatrimonio :: Ciudadano -> Ciudadano -> Ciudadano
menorPatrimonio unCiudadano otroCiudadano | patrimonio unCiudadano < patrimonio otroCiudadano = unCiudadano
                                          | otherwise = otroCiudadano

{-ghci> diferenciaDePatrimonio springfield
2011000.0
-}

tieneAutoAltaGama :: Ciudadano -> Bool
tieneAutoAltaGama unCiudadano = any gamaAlta. bienes $ unCiudadano

gamaAlta :: Bien -> Bool
gamaAlta ("auto", valor) = valor > 100000
gamaAlta _ = False

{-ghci> tieneAutoAltaGama burns
True
-}


type Medida = Ciudadano -> Ciudadano

auh :: Medida
auh ciudadano = aplicarMedidaSegun (patrimonio ciudadano < 0) (modificarSueldo ((incremento.cantidadDeHijos) ciudadano)) ciudadano


aplicarMedidaSegun :: Bool -> (Ciudadano -> Ciudadano) -> Ciudadano -> Ciudadano
aplicarMedidaSegun condicion f ciudadano | condicion = f ciudadano
                                         | otherwise = ciudadano

modificarSueldo :: Float -> Ciudadano -> Ciudadano
modificarSueldo cantidad ciudadano = ciudadano {sueldo = sueldo ciudadano + cantidad}

incremento :: Float -> Float
incremento cantidad = 1000 * cantidad


impuestoGanancias :: Float -> Medida
impuestoGanancias minimo ciudadano = aplicarMedidaSegun (sueldo ciudadano > minimo) (modificarSueldo (diferencia minimo (sueldo ciudadano))) ciudadano 

diferencia:: Float -> Float -> Float
diferencia minimo sueldo = (minimo - sueldo) * 0.3

{-ghci> impuestoGanancias 40000 burns
UnCiudadano {profesion = "Empresario", sueldo = 222000.0, cantidadDeHijos = 1.0, bienes = 
[("empresa",1000000.0),("empresa",500000.0),("auto",200000.0)]}
-}


impuestoAltaGama :: Medida
impuestoAltaGama ciudadano = aplicarMedidaSegun (tieneAutoAltaGama ciudadano) (modificarSueldo ((impuesto.bienes)ciudadano)) ciudadano

impuesto :: [Bien] -> Float
impuesto bienes = (*(-0.1)) .snd.head.filter gamaAlta $ bienes

{-ghci> impuestoAltaGama burns
UnCiudadano {profesion = "Empresario", sueldo = 280000.0, cantidadDeHijos = 1.0, bienes = 
[("empresa",1000000.0),("empresa",500000.0),("auto",200000.0)]}
-}

negociarSueldoProfesion :: String -> Float -> Medida
negociarSueldoProfesion unaProfesion porcentaje ciudadano = aplicarMedidaSegun ((==unaProfesion).profesion $ ciudadano) (modificarSueldo (aumento porcentaje (sueldo ciudadano))) ciudadano

aumento :: Float -> Float -> Float
aumento porcentaje sueldo = (sueldo * porcentaje)/ 100

{-ghci> negociarSueldoProfesion "Profesor" 30 frink
UnCiudadano {profesion = "Profesor", sueldo = 15600.0, cantidadDeHijos = 1.0, bienes = []}
-}


data Gobierno = UnGobierno {años :: [Float], medidas :: [Medida]}

gobiernoA :: Gobierno
gobiernoA = UnGobierno [1999..2003] [impuestoGanancias 30000, negociarSueldoProfesion "Profesor" 10,negociarSueldoProfesion "Empresario" 40, impuestoAltaGama, auh ]

gobiernoB = UnGobierno [2004..2008] [impuestoGanancias 40000, negociarSueldoProfesion "Profesor" 30, negociarSueldoProfesion "Camionero" 40]

gobernarUnAño :: Gobierno -> Ciudad -> Ciudad
gobernarUnAño gobierno ciudad = map (aplicarMedidas gobierno) ciudad

aplicarMedidas :: Gobierno -> Ciudadano -> Ciudadano
aplicarMedidas gobierno ciudadano = foldl (flip ($)) ciudadano. medidas $ gobierno

gobernarPeriodoCompleto :: Gobierno -> Ciudad  -> Ciudad
gobernarPeriodoCompleto gobierno ciudad = foldl (\unaCiudad _ -> gobernarUnAño gobierno unaCiudad)  ciudad. años $ gobierno

distribuyoRiqueza :: Gobierno -> Ciudad -> Bool
distribuyoRiqueza gobierno ciudad = diferenciaDePatrimonio ciudad > (diferenciaDePatrimonio.gobernarPeriodoCompleto gobierno) ciudad

{-ghci> distribuyoRiqueza gobiernoA springfieldTrue
-}
-- 
kane = UnCiudadano "Empresario" 100000 0 infinitosTrineos

infinitosTrineos = [("Rosebud", valor)|  valor <- [5,10..]]


f1 :: (Num a) =>  a -> ( b -> a  -> Bool ) ->  b  -> [ a ]  -> [ a ] 
f1 x y z = map (*x) . filter (y z)
