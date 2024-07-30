module Library where
import PdePreludat

{------------------------------------EJERCICIO PDEPOP BANDAS DE MUSICA---------------------------------

data Cancion = Cancion {
titulo :: String,
duracion :: Number,
instrumentos :: [Instrumento]
} deriving (Show)

data Instrumento = Guitarra | Bajo | Bateria | Teclado deriving (Show, Eq)


patternMatching :: Cancion
patternMatching = Cancion "Pattern Matching" 4 [Guitarra, Bajo, Bateria]

seisDieciocho :: Cancion
seisDieciocho = Cancion "Seis Dieciocho (18)" 3 [Teclado, Guitarra]

laVidaEnHaskell :: Cancion
laVidaEnHaskell = Cancion "La vida en haskell" 5 []

mothToAFlame :: Cancion
mothToAFlame = Cancion "Moth to a Flame" 4 [Guitarra, Bajo, Teclado]

starboy :: Cancion
starboy = Cancion "Starboy" 4 [Guitarra, Bajo, Teclado]

cancionesOriginales :: [Cancion]
cancionesOriginales = [patternMatching, seisDieciocho, laVidaEnHaskell]


comienzaConM :: Cancion -> Bool
comienzaConM cancion = head (titulo cancion) == 'M'

duracionPar :: Cancion -> Bool
duracionPar cancion = even (duracion cancion)

cantidadLetrasPorDiez :: Cancion -> Number
cantidadLetrasPorDiez cancion = length (titulo cancion) *10

esAcapella :: Cancion -> Bool
esAcapella cancion = null (instrumentos cancion) 

aceptacion :: Cancion -> Number
aceptacion cancion
 | comienzaConM cancion = 500
 | duracionPar cancion = cantidadLetrasPorDiez cancion
 | esAcapella cancion = 10
 | otherwise = 0


repertorio :: [Cancion]
repertorio = [patternMatching, seisDieciocho, laVidaEnHaskell, mothToAFlame, starboy]


vieneAntes :: Cancion -> Cancion -> Cancion
vieneAntes cancion1 cancion2 
 | primeraLetra cancion1 > primeraLetra cancion2 = cancion1
 | otherwise = cancion2

primeraLetra :: Cancion -> Char
primeraLetra cancion = head (titulo cancion)

esAceptadaPorElPublico :: Cancion -> Bool
esAceptadaPorElPublico cancion = aceptacion cancion > 60

necesitaAlInstrumento :: Cancion -> Instrumento -> Bool
necesitaAlInstrumento cancion instrumento = elem instrumento (instrumentos cancion)

tocarUnaCancion :: Cancion -> Cancion
tocarUnaCancion cancion
 | esAceptadaPorElPublico cancion = cancion
 | otherwise = cancion {duracion = (duracion cancion) / 2}
-}

{-------------------------------------------EJERCICIO CUMPLEAÑITOS--------------------------------
data Invitado = Invitado {
    nombre :: String,
    nivelDeCansancio :: Number,
    nivelDeFelicidad :: Number,
    cancionFavorita :: String
} deriving (Show, Eq)

estaCansada :: Invitado -> Bool
estaCansada invitado = nivelDeCansancio invitado > 80

cansarse :: Invitado -> Invitado
cansarse invitado = invitado {nivelDeCansancio = (nivelDeCansancio invitado) + 10}

disfrutar :: Invitado -> Invitado
disfrutar invitado = invitado {nivelDeFelicidad = (nivelDeFelicidad invitado) + 100 - (nivelDeCansancio invitado)}


type Plancito = Invitado -> Invitado

charlaDeFutbol :: Plancito
charlaDeFutbol invitado = disfrutar invitado

bailar :: Plancito
bailar invitado = cansarse (disfrutar invitado)

mesaDulce :: Plancito
mesaDulce invitado = disfrutar (cansarse invitado)

tieneBuenGusto :: Invitado -> Bool
tieneBuenGusto invitado = even (length (cancionFavorita invitado))

leVaADarFiaca :: Invitado -> Plancito -> Bool
leVaADarFiaca invitado plancito = estaCansada (plancito invitado)


type Bandurria = [Invitado]

playlist :: Bandurria -> [String]
playlist bandurria = map cancionFavorita bandurria

losQueLaSiguen :: Bandurria -> Bandurria
losQueLaSiguen bandurria = filter (not.estaCansada) bandurria

bandurriaDespuesDePlancito :: Bandurria -> Plancito -> Bandurria
bandurriaDespuesDePlancito bandurria plancito = map plancito bandurria

laRompe :: Bandurria -> Bool
laRompe bandurria = all estaParaSeguirla bandurria

estaParaSeguirla :: Invitado -> Bool
estaParaSeguirla invitado = (not.estaCansada) invitado

hitazo :: Bandurria -> Bool
hitazo bandurria = any tieneBuenGusto bandurria

sumaDeFelicidades :: Bandurria -> Number
sumaDeFelicidades bandurria = sum (map nivelDeFelicidad (losQueLaSiguen bandurria))

fieston :: Bandurria -> Bool
fieston bandurria = (sumaDeFelicidades bandurria) > 300

seLaSube :: Bandurria -> Plancito -> Bool
seLaSube bandurria plancito = sum (map nivelDeFelicidad (bandurriaDespuesDePlancito bandurria plancito)) >= 300
-}

{--------------------------------------EJERCICIO JUJUTSU HASKELL----------------------------
data Hechicero = Hechicero {
    nombre :: String,
    antiguedad :: Number,
    clan :: String,
    grado :: Number
} deriving (Show,Eq)

nobara :: Hechicero
nobara = Hechicero "Nobara" 1 "Kugisaki" 3

satoru :: Hechicero
satoru = Hechicero "Satoru" 15 "Gojo" 0

maki :: Hechicero
maki = Hechicero "Maki" 3 "Zenin" 4

yuji :: Hechicero
yuji = Hechicero "Yuji" 0 "Itadori" 1

grupoA :: [Hechicero]
grupoA = [yuji, maki]

grupoB :: [Hechicero]
grupoB = [nobara, satoru]

grupoC :: [Hechicero]
grupoC = [nobara, satoru, yuji, maki]

grupoPreparado :: [Hechicero] -> Bool
grupoPreparado grupo = length grupo > 3

grupoInvencible :: [Hechicero] -> Bool
grupoInvencible grupo = any esDeGradoEspecial grupo

esDeGradoEspecial :: Hechicero -> Bool
esDeGradoEspecial hechicero = (==0)(grado hechicero)

esPrestigioso :: Hechicero -> Bool
esPrestigioso hechicero = (clan hechicero) == "Zenin" || (clan hechicero) == "Gojo" || (clan hechicero) == "Kamo"

grupoFavorito :: [Hechicero] -> Bool
grupoFavorito grupo = all esPrestigioso grupo

esExperto :: Hechicero -> Bool
esExperto hechicero = (>1)(antiguedad hechicero)

sonExpertos :: [Hechicero] -> [Hechicero]
sonExpertos grupo = filter esExperto grupo

subeDeGrado :: Hechicero -> Hechicero
subeDeGrado hechicero
 | grado hechicero > 0 = hechicero {grado = grado hechicero - 1}
 | otherwise = hechicero

grupoHaceFrenteACualquierMaldicion :: [Hechicero] -> Bool
grupoHaceFrenteACualquierMaldicion grupo = grupoInvencible grupo || grupoPreparado grupo

powerUp :: [Hechicero] -> [Hechicero]
powerUp grupo = map subeDeGrado grupo


nivelTryhard :: Hechicero -> Number
nivelTryhard hechicero = 1 / (grado hechicero + 1)

mayorNivelTryhard :: Hechicero -> Hechicero -> Hechicero
mayorNivelTryhard hechicero1 hechicero2
 | nivelTryhard hechicero1 > nivelTryhard hechicero2 = hechicero1
 | otherwise = hechicero2

nivelBurocratico :: Hechicero -> Number
nivelBurocratico hechicero = length (nombre hechicero)

mayorNivelBurocratico :: Hechicero -> Hechicero -> Hechicero
mayorNivelBurocratico hechicero1 hechicero2
 | nivelBurocratico hechicero1 > nivelBurocratico hechicero2 = hechicero1
 | otherwise = hechicero2

nivelIntimidante :: Hechicero -> Char
nivelIntimidante hechicero = maximum (clan hechicero)

mayorNivelIntimidante :: Hechicero -> Hechicero -> Hechicero
mayorNivelIntimidante hechicero1 hechicero2 
 | nivelIntimidante hechicero1 > nivelIntimidante hechicero2 = hechicero1
 | otherwise = hechicero2

nivelDeSigilo :: Hechicero -> Number
nivelDeSigilo hechicero = (antiguedad hechicero) * 6

mayorNivelDeSigilo :: Hechicero -> Hechicero -> Hechicero
mayorNivelDeSigilo hechicero1 hechicero2
 | nivelDeSigilo hechicero1 > nivelDeSigilo hechicero2 = hechicero1
 | otherwise = hechicero2
-}

{--------------------------------------EJERCICIO ARTISTAS-------------------------------
type Cancion = String

data Artista = Artista {
    nombre :: String,
    canciones :: [Cancion]
} deriving Show

fitito :: Artista
fitito = Artista "Fitito Paez" ["11 y 6", "El amor despues del amor", "Mariposa Tecknicolor"]

calamardo :: Artista
calamardo = Artista "Andres Calamardo" ["Flaca", "Sin Documentos", "Tuyo siempre"]

paty :: Artista
paty = Artista "Taylor Paty" ["Shake It Off", "Lover"]

theWeeknd :: Artista
theWeeknd = Artista "The Weeknd" ["Moth to a Flame", "House of Balloons", "Party Monster", "Is There Someone Else?", "Faith", "I Was Never There", "Blinding Lights"]

calificacionDeCancion :: Cancion -> Number
calificacionDeCancion cancion = length (letrasMinusculas cancion) + 10

letrasMinusculas :: Cancion -> String
letrasMinusculas cancion = filter esMinuscula cancion

esMinuscula :: Char -> Bool
esMinuscula letra = elem letra ['a'..'z']


tieneBuenaCalificacion :: Cancion -> Bool
tieneBuenaCalificacion cancion = (calificacionDeCancion cancion) > 20

cancionesBuenas :: Artista -> [Cancion]
cancionesBuenas artista = filter tieneBuenaCalificacion (canciones artista)

esExitoso :: Artista -> Bool
esExitoso artista = sum (map calificacionDeCancion (cancionesBuenas artista)) > 20

artistasExitosos :: [Artista] -> [Artista]
artistasExitosos artistas = filter esExitoso artistas
-}

{--------------------------------------EJERCICIO POKEMON---------------------------------------
data Pokemon = Pokemon {
    nombre :: String,
    tipo :: Tipo
} deriving (Show, Eq)

data Tipo = Agua | Planta | Fuego | Tierra | Otro deriving (Show, Eq)

charmander :: Pokemon
charmander = Pokemon "Charmander" Fuego

gyarados :: Pokemon
gyarados = Pokemon "Gyarados" Agua

tauros :: Pokemon
tauros = Pokemon "Tauros" Planta

carpincho :: Pokemon
carpincho = Pokemon "Carpincho" Tierra

bulbasaur :: Pokemon
bulbasaur = Pokemon "Bulbasaur" Planta

oddish :: Pokemon
oddish = Pokemon "Oddish" Planta

squirtle :: Pokemon
squirtle = Pokemon "Squirtle" Agua

grupoPlanta :: [Pokemon]
grupoPlanta = [tauros, oddish, bulbasaur]


leGanaA :: Tipo -> Tipo
leGanaA tipo
  | tipo == Agua   = Fuego
  | tipo == Planta = Agua
  | tipo == Fuego  = Planta
  | tipo == Tierra = Planta
  | otherwise = Otro

quienGana :: Pokemon -> Pokemon -> Pokemon
quienGana pokemon1 pokemon2 
 | leGanaA (tipo pokemon1) == tipo pokemon2 = pokemon1
 | otherwise = pokemon2


aQuienesLeGana :: Pokemon -> [Pokemon] -> [Pokemon]
aQuienesLeGana pokemon pokemones = filter (leGana pokemon) pokemones

leGana :: Pokemon -> Pokemon -> Bool
leGana pokemon1 pokemon2 = leGanaA (tipo pokemon1) == tipo pokemon2

aCuantosLeGana :: Pokemon -> [Pokemon] -> Number
aCuantosLeGana pokemon pokemones = length (aQuienesLeGana pokemon pokemones)


leGanaAMasPokemones :: [Pokemon] -> Pokemon -> Pokemon -> Pokemon
leGanaAMasPokemones pokemones pokemon1 pokemon2
 | aCuantosLeGana pokemon1 pokemones > aCuantosLeGana pokemon2 pokemones = pokemon1
 | otherwise = pokemon2

elMasPicante :: [Pokemon] -> Pokemon
elMasPicante pokemones = foldl1 (leGanaAMasPokemones pokemones) pokemones


data Destino = UnGimnasio {nombreGym:: String, siguiente:: Destino}
                   | UnaLiga {contrincantes:: [Pokemon] } deriving (Show, Eq)

estaAlHorno :: Pokemon -> Destino -> Bool
estaAlHorno pokemon destino
 | esUnGimnasio destino = True
 | esUnaLiga destino && all (pierdeContra pokemon) (contrincantes destino) = True
 | otherwise = False

esUnGimnasio :: Destino -> Bool
esUnGimnasio (UnGimnasio _ _) = True
esUnGimnasio _ = False

esUnaLiga :: Destino -> Bool
esUnaLiga (UnaLiga _) = True
esUnaLiga _ = False

pierdeContra :: Pokemon -> Pokemon -> Bool
pierdeContra pokemon1 pokemon2 = leGana pokemon2 pokemon1

gymRoca :: Destino
gymRoca = UnGimnasio "Gym Roca" gymAgua

gymAgua :: Destino
gymAgua = UnGimnasio "Gym Agua" gymElectrico

gymElectrico :: Destino
gymElectrico = UnGimnasio "Gym Eléctrico" ligaKanto

ligaKanto :: Destino
ligaKanto = UnaLiga [charmander, tauros, carpincho]


gymFuego :: Destino
gymFuego = UnGimnasio "Gym Fuego" gymPlanta

gymPlanta :: Destino
gymPlanta = UnGimnasio "Gym Planta" ligaGali

ligaGali :: Destino
ligaGali = UnaLiga grupoPlanta

puedeViajar :: Destino -> Destino -> Bool
puedeViajar origen destino
 | esUnaLiga origen = False
 | esUnGimnasio origen = (origen == destino) || puedeViajar (siguiente origen) destino
-}

{------------------------------------Ejemplos Recursividad------------------------------------
head :: [a] -> a
head (cabeza : cola) = cabeza

null :: [a] -> Bool
--caso base
null [] = True
null (cabeza : cola) = False

factorial :: Number -> Number
--caso base
factorial 0 = 1
--caso recursivo
factorial n = n * factorial (n-1)
-- va a hacer n*n-1 hasta que n sea 0

largo :: [a] -> Number
largo [] = 0
largo (cabeza:cola) = 1 + largo cola

ultimo :: [a] -> a
ultimo [a] = a
ultimo (cabeza:cola) = ultimo cola

segundo :: [a] -> a
segundo (primero : segundo : cola) = segundo

algunoCumple :: (a -> Bool) -> [a] -> Bool
algunoCumple _ [] = False
algunoCumple f (cabeza:cola) = f cabeza || algunoCumple f cola

todosCumplen :: (a -> Bool) -> [a] -> Bool
todosCumplen _ [] = True
todosCumplen f (cabeza:cola) = f cabeza && todosCumplen f cola

maximo :: (Ord a) => [a] -> a
maximo [a] = a
maximo (cabeza:cola) = foldl max cabeza cola

repetir :: a -> Number -> [a]
repetir x 0 = []
repetir x n = x : repetir x (n - 1) 
-}
 
{----------------------------------Ejemplos Expresiones Lambda------------------------------
data Persona = Persona {
    nombre :: String,
    edad :: Number,
    sueldo :: Number
}

sumaUnAño personas = map (\persona -> edad persona +1) personas

personasConEdadPar personas = filter (\persona -> even (edad persona)) personas 

sumarEdadesDePersonas personas = foldl (\edadNula persona -> edadNula + edad persona) 0 personas

personasMayores personas = filter (\persona -> edad persona > 18) personas

quienesSuperanNMonto personas n = filter (\persona -> sueldo persona > n) personas
-}


--1--
data Autor = Autor {
    nombreAutor :: String,
    obras :: [Obra]
} deriving (Show, Eq)

data Obra = Obra {
    nombreObra :: String,
    contenido :: String,
    publicacion :: Number
} deriving (Show, Eq)

obraA :: Obra
obraA = Obra "Había una vez un pato." "texto de habia una vez un pato" 1997

obraB :: Obra
obraB = Obra "¡Habia una vez un pato!" "texto de habia una vez un pato 2" 1996

obraC :: Obra
obraC = Obra "Mirtha, Susana y Moria" "texto de mirta, susana y moria" 2010

obraD :: Obra
obraD = Obra "La semántica funcional del amoblamiento vertebral es riboficiente" "texto de la semántica funcional del amoblamiento vertebral es riboficiente" 2020

obraE :: Obra
obraE = Obra "La semántica funcional de Mirtha, Susana y Moria." "texto de la semántica funcional de Mirtha, Susana y Moria." 2022

--2--
esLetra :: Char -> Bool
esLetra caracter = elem caracter ['a' .. 'z'] || elem caracter ['A' .. 'Z'] || elem caracter ['Á' .. 'Ú'] || elem caracter ['á' .. 'ú']

esNumero :: Char -> Bool
esNumero caracter = elem caracter ['0' .. '9']

esEspacio :: Char -> Bool
esEspacio caracter = elem caracter [' ']

palabraSinTildes :: String -> String
palabraSinTildes palabra = map letraSinTilde palabra

letraSinTilde :: Char -> Char
letraSinTilde letra
 | letra == 'á' = 'a'
 | letra == 'é' = 'e'
 | letra == 'í' = 'i'
 | letra == 'ó' = 'o'
 | letra == 'ú' = 'u'
 | otherwise = letra

esLetraONumeroOEspacio :: Char -> Bool
esLetraONumeroOEspacio caracter = (esLetra caracter) || (esNumero caracter) || (esEspacio caracter)

versionCruda :: Obra -> Obra
versionCruda obra = obra {nombreObra = filter esLetraONumeroOEspacio (palabraSinTildes (nombreObra obra))}

--3--
esCopiaLiteral :: TipoDePlagio
esCopiaLiteral obra1 obra2 = versionCruda obra2 == versionCruda obra1

empiezaIgual :: Number -> TipoDePlagio
empiezaIgual n obra1 obra2 = take n (nombreObra obra2) == take n (nombreObra obra1)

noEmpiezaIgual :: Number -> TipoDePlagio
noEmpiezaIgual n obra1 obra2 = not (empiezaIgual n obra1 obra2)

leAgregaronInfo :: TipoDePlagio
leAgregaronInfo obra1 obra2 = noEmpiezaIgual (length (nombreObra obra2)) obra1 obra2 && empiezaIgual (length (nombreObra obra2) - length (nombreObra obra1)) obra1 obra2

mismaCantidadDeLetras :: Obra -> Obra -> Bool
mismaCantidadDeLetras obra1 obra2 = length (nombreObra obra1) == length (nombreObra obra2)

mismaCantidadDeLetrasLambda :: TipoDePlagio
mismaCantidadDeLetrasLambda obra1 obra2 = (\nombre1 nombre2 -> length nombre1 == length nombre2) (nombreObra obra1) (nombreObra obra2)

type TipoDePlagio = Obra -> Obra -> Bool

--4--
data Bot = Bot {
    plagiosQueDetecta :: [TipoDePlagio],
    fabricante :: String
} deriving (Show, Eq)

botA :: Bot
botA = Bot [esCopiaLiteral, empiezaIgual 10] "Fabricante A"

botB :: Bot
botB = Bot [leAgregaronInfo, mismaCantidadDeLetrasLambda] "Fabricante B"

--5--
detectaPlagio :: Obra -> Obra -> TipoDePlagio -> Bool
detectaPlagio obra1 obra2 plagio = plagio obra1 obra2 && publicacion obra1 > publicacion obra2

botDetectaPlagio :: Bot -> Obra -> Obra -> Bool
botDetectaPlagio bot obra1 obra2 = any (detectaPlagio obra1 obra2) (plagiosQueDetecta bot)

--6--
-- si una obra es plagio de alguna de las obras del autor
obraPlagioAlAutor :: Bot -> Autor -> Obra -> Bool
obraPlagioAlAutor bot autor obraCopia = any (botDetectaPlagio bot obraCopia) (obras autor)

--si un autor hizo plagio a otro:
autorPlagioAlOtroAutor :: Bot -> Autor -> Autor -> Bool
autorPlagioAlOtroAutor bot autor1 autor2 = any (obraPlagioAlAutor bot autor1) (obras autor2)

esCadenaDePlagiadores :: [Autor] -> Bot -> Bool
esCadenaDePlagiadores [] _ = False
esCadenaDePlagiadores (autor1:autor2:restoDeAutores) bot = autorPlagioAlOtroAutor bot autor1 autor2 && esCadenaDePlagiadores (autor2:restoDeAutores) bot

--7--
aCuantosPlagioUnAutor :: Bot -> Autor -> [Autor] -> Number
aCuantosPlagioUnAutor bot autorPlagiador autores = length (filter (autorPlagioAlOtroAutor bot autorPlagiador) autores)

hizoPlagioPeroAprendio :: Bot -> [Autor] -> Autor -> Bool
hizoPlagioPeroAprendio bot autores autorQueAprendio= (aCuantosPlagioUnAutor bot autorQueAprendio autores) == 1

hicieronPlagioPeroAprendieron :: Bot -> [Autor] -> [Autor]
hicieronPlagioPeroAprendieron bot autores = filter (hizoPlagioPeroAprendio bot autores) autores

--8--
obraInfinita :: Obra -> Obra
obraInfinita obra = obra {contenido = cycle (contenido obra)}

obraInfinita2 :: Obra
obraInfinita2 = Obra "Obra con contenido infinito" (repeat 'c') 2024

--si el plagio fuera :
-- Copia Literal = tiraría error, ya que nunca terminaría de comparar los nombres de las obras
-- Empiezan Igual = podría comparar los n primeros caracteres entre las obras (gracias a la lazy evaluation) y devolvería true o false segun la longitud de las obras, ya que una vez una obra llegue a tener menor longitud que la otra, deja de evaluar y retorna true.
-- Le Agregaron Info = podría comparar el comienzo de las obras y retornaría un valor, pero al ser infinita, nunca terminaría de comparar el resto del texto con el de la obra original (no sabemos como terminaría la obra infinita)