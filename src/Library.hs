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

{------------------------------------------PARCIAL 6/6/2024------------------------------------
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
-}

{---------------------------------------------PARCIAL HARRY POSTRE--------------------------------
--1--
data Postre = Postre{
    nombrePostre :: String,
    sabores :: [Sabor],
    peso :: Number,
    temperatura :: Number
} deriving(Show,Eq)

type Sabor = String

bizcochoBorracho :: Postre
bizcochoBorracho = Postre "Bizcocho Borracho" ["Fruta", "Crema"] 100 25

--2--
type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio postre = postre {temperatura = temperatura postre + 1, peso = peso postre + (peso postre * 0.05)}

immobulus :: Hechizo
immobulus postre = postre {temperatura = 0}

wingardiumLeviosa :: Hechizo
wingardiumLeviosa postre = postre {sabores = sabores postre ++ ["Concentrado"], peso = peso postre - (peso postre * 0.1)}

diffindo :: Number -> Hechizo
diffindo n postre = postre {peso = peso postre - (peso postre * n/100)}

riddikulus :: Sabor -> Hechizo
riddikulus sabor postre = postre {sabores = sabores postre ++ [(inviertePalabra sabor)]}

inviertePalabra :: String -> String
inviertePalabra palabra = reverse palabra

avadaKedavra :: Hechizo
avadaKedavra postre = immobulus (postre {sabores = []})

--3--
estanListosConUnHechizo :: [Postre] -> Hechizo -> Bool
estanListosConUnHechizo postres hechizo = estanListos (map hechizo postres)

estaListo :: Postre -> Bool
estaListo postre = (peso postre) > 0 && (sabores postre) /= [] && (temperatura postre) /= 0

estanListos :: [Postre] -> Bool
estanListos postres = all estaListo postres


apicarleUnHechizoAUnPostre :: Postre -> Hechizo -> Postre
apicarleUnHechizoAUnPostre postre hechizo = hechizo (postre)

aplicarleUnHechizoAVariosPostres :: [Postre] -> Hechizo -> [Postre]
aplicarleUnHechizoAVariosPostres postres hechizo = map hechizo postres

--4--
postresListos :: [Postre] -> [Postre]
postresListos postres = filter (estaListo) postres

postresListosDeLaMesa :: [Postre] -> Hechizo -> [Postre]
postresListosDeLaMesa postres hechizo = filter estaListo (map hechizo postres)

--5--
data Mago = Mago {
    nombre :: String,
    hechizosAprendidos :: [Hechizo],
    horrorCruxes :: Number
} deriving (Show,Eq)

harry :: Mago
harry = Mago "Harry Potter" [incendio, immobulus, riddikulus "Chocolate"] 10

hermione :: Mago
hermione = Mago "Hermione Granger" [wingardiumLeviosa, diffindo 50, avadaKedavra] 100

--6--
practicaConUnHechizo :: Mago -> Postre -> Hechizo -> Mago
practicaConUnHechizo mago postre hechizo
 | (hechizo postre) == (avadaKedavra postre) = mago {hechizosAprendidos = hechizosAprendidos mago ++ [hechizo], horrorCruxes = horrorCruxes mago + 1}
 | otherwise = mago {hechizosAprendidos = hechizosAprendidos mago ++ [hechizo]}

--7--
suMejorHechizo :: Postre -> Mago -> [Hechizo] -> Hechizo 
suMejorHechizo postre mago (hechizo1:restoDeHechizos) = foldl (mejorHechizo postre) hechizo1 restoDeHechizos

mejorHechizo :: Postre -> Hechizo -> Hechizo -> Hechizo
mejorHechizo postre hechizo1 hechizo2
 | length (sabores (hechizo1(postre))) > length (sabores (hechizo2(postre))) = hechizo1
 | otherwise = hechizo2

--8--
listaDePostresInfinitos :: Postre -> [Postre]
listaDePostresInfinitos postre = cycle [postre]

magoConInfinitosHechizos :: Mago -> Mago
magoConInfinitosHechizos mago = mago {hechizosAprendidos = repeat avadaKedavra}
-}

{---------------------------------------------PARCIAL FMS----------------------------------------
type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String -- Solamente interesa el nombre

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

--1--
vocalesDePalabra :: Palabra -> Palabra
vocalesDePalabra palabra = filter esVocal palabra

ultimasNVocales :: Number -> Palabra -> Palabra
ultimasNVocales n palabra = drop (length palabra - n) (vocalesDePalabra palabra)

rimaAsonante :: Palabra -> Palabra -> Bool
rimaAsonante p1 p2 = ultimasNVocales 1 p1 == ultimasNVocales 1 p2

ultimasNLetras :: Number -> Palabra -> Palabra
ultimasNLetras n palabra = drop (length palabra - n) palabra

rimaConsonante :: Palabra -> Palabra -> Bool
rimaConsonante p1 p2 = ultimasNLetras 3 p1 == ultimasNLetras 3 p2

rimanPalabras :: Palabra -> Palabra -> Bool
rimanPalabras p1 p2
 | p1 == p2 = False
 | rimaConsonante p1 p2 = True
 | rimaAsonante p1 p2 = True
 | otherwise = False

--2--
verso1 :: Verso
verso1 = "no hace falta un programa que genere una canción"

verso2 :: Verso
verso2 = "para saber que esto se resuelve con una función"

verso3 :: Verso
verso3 = "funcion linda y divertida si rendiste todas las katas"

porMedioDeRimas :: Verso -> Verso -> Bool
porMedioDeRimas v1 v2 = last (words v1) == last (words v2)

porMedioDeAnadiplosis :: Verso -> Verso -> Bool
porMedioDeAnadiplosis v1 v2 = last (words v1) == head (words v2)

rimanVersos :: Verso -> Verso -> Bool
rimanVersos v1 v2 = porMedioDeRimas v1 v2 || porMedioDeAnadiplosis v1 v2
-}

{-----------------------------------------------PARCIAL HUBER 2.0------------------------------------------
--1--
data Chofer = Chofer {
    nombreChofer :: String,
    kilometraje :: Number,
    viajes :: [Viaje],
    condicion :: Condicion
} deriving (Show,Eq)

data Cliente = Cliente {
    nombreCliente :: String,
    barrio :: Barrio
} deriving (Show, Eq)

data Viaje = Viaje {
    fecha :: String,
    costo :: Number,
    cliente :: Cliente
} deriving (Show, Eq)

type Barrio = String
type Condicion = Viaje -> Bool

--2--
cualquierViaje :: Condicion
cualquierViaje viaje = True

masDe200Pesos :: Condicion
masDe200Pesos viaje = costo viaje > 200

nombreConMasDeNLetras :: Number -> Condicion
nombreConMasDeNLetras n viaje = length (nombreCliente (cliente viaje)) > n

noViveEnUnaZonaDeterminada :: Barrio -> Condicion
noViveEnUnaZonaDeterminada barrioDeterminado viaje = barrio (cliente viaje) /= barrioDeterminado

--3--
lucas :: Cliente
lucas = Cliente "Lucas" "Victoria"

daniel :: Chofer
daniel = Chofer "Daniel" 23500 [viaje1] (noViveEnUnaZonaDeterminada "Olivos")

viaje1 :: Viaje
viaje1 = Viaje "20/04/2017" 150 lucas

alejandra :: Chofer
alejandra = Chofer "Alejandra" 180000 [] cualquierViaje

--4--
puedeTomarViaje :: Viaje -> Chofer -> Bool
puedeTomarViaje viaje chofer = (condicion chofer) viaje == True

--5--
liquidacionDeChofer :: Chofer -> Number
liquidacionDeChofer chofer = sum (map costo (viajes chofer))

--6--
quienesPuedenRealizarViaje :: Viaje -> [Chofer] -> [Chofer]
quienesPuedenRealizarViaje viaje choferes = filter (puedeTomarViaje viaje) choferes

quienesPuedenRealizarViaje' :: [Chofer] -> Viaje -> [Chofer]
quienesPuedenRealizarViaje' choferes viaje = filter (\chofer -> puedeTomarViaje viaje chofer) choferes


choferConMenosViajes :: Chofer -> Chofer -> Chofer
choferConMenosViajes chofer1 chofer2
 | length (viajes chofer1) < length (viajes chofer2) = chofer1
 | otherwise = chofer2

choferConMenosExperiencia :: [Chofer] -> Chofer
choferConMenosExperiencia choferes = foldl1 choferConMenosViajes choferes

--supongo que quien realiza el viaje es de los que lo pueden realizar, el que tiene menor experiencia
realizaViaje :: Viaje -> [Chofer] -> Chofer
realizaViaje viaje choferes = choferConMenosExperiencia (quienesPuedenRealizarViaje viaje choferes)


efectuarViaje :: Viaje -> Chofer -> Chofer
efectuarViaje viaje chofer = chofer {viajes = viajes chofer ++ [viaje] }

--7--
nitoInfy :: Chofer
nitoInfy = Chofer "Nito Infy" 70000 (repetirViaje viajeInfinito) (nombreConMasDeNLetras 2)

viajeInfinito :: Viaje
viajeInfinito = Viaje "11/3/2017" 50 lucas

repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

-- no se puede calcular la liquidacion de nito ya que nunca se termina de sumar el costo de los viajes (al ser infinitos).
-- sí podemos saber si Nito puede tomar un viaje con Lucas de $ 500 el 2/5/2017, ya que con solo cumplir la condicion que pide nito (que el nombre de su cliente tenga al menos 2 letras) ya nos da una respuesta : sí puede.
-}

{------------------------------------------------PARCIAL STAR WARS-----------------------------------------
--1--
data Nave = Nave {
    nombre :: String,
    durabilidad :: Number,
    escudo :: Number,
    ataque :: Number,
    poder :: Poder
} deriving (Show, Eq)

type Poder = Nave -> Nave

tieFighter :: Nave
tieFighter = Nave "Tie Fighter" 200 100 50 movimientoTurbo

xWing :: Nave
xWing = Nave "X Wing" 300 150 100 reparacionEmergencia

naveDeDarthVader :: Nave
naveDeDarthVader = Nave "Nave de Darth Vader" 500 300 200 movimientoSuperTurbo

milleniumFalcon :: Nave
milleniumFalcon = Nave "Millenium Falcon" 1000 500 50 reparacionEmergenciaEIncrementoDeEscudo

movimientoTurbo :: Poder
movimientoTurbo nave = nave {ataque = ataque nave + 25}

reparacionEmergencia :: Poder
reparacionEmergencia nave = nave {durabilidad = durabilidad nave + 50, ataque = ataque nave - 30}

movimientoSuperTurbo :: Poder
movimientoSuperTurbo nave = movimientoTurbo(movimientoTurbo(movimientoTurbo(nave {durabilidad = durabilidad nave - 45})))

reparacionEmergenciaEIncrementoDeEscudo :: Poder
reparacionEmergenciaEIncrementoDeEscudo nave = reparacionEmergencia (nave {escudo = escudo nave + 100})

--2--
type Flota = [Nave]

durabilidadTotal :: Flota -> Number
durabilidadTotal flota = sum (map durabilidad flota)

--3--
naveFueAtacada :: Nave -> Nave -> Nave
naveFueAtacada naveAtacante naveAtacada
 | (escudo (activaSuPoder naveAtacada)) < (ataque(activaSuPoder naveAtacante)) = naveAtacada {durabilidad = durabilidad naveAtacada - (ataque naveAtacante - escudo naveAtacada)}
 | otherwise = naveAtacada

activaSuPoder :: Nave -> Nave
activaSuPoder nave = (poder nave) nave

--4--
fueraDeCombate :: Nave -> Bool
fueraDeCombate nave = durabilidad nave == 0

--5--
type Estrategia = Flota -> Flota

naveDebil :: Nave -> Bool
naveDebil nave = escudo nave < 200

navesDebiles :: Estrategia
navesDebiles flota = filter naveDebil flota


naveConCiertaPeligrosidad :: Number -> Nave -> Bool
naveConCiertaPeligrosidad n nave = ataque nave > n

navesConCiertaPeligrosidad :: Number -> Estrategia
navesConCiertaPeligrosidad n flota = filter (naveConCiertaPeligrosidad n) flota


naveQueQuedarianFueraDeCombate :: Nave -> Nave -> Bool
naveQueQuedarianFueraDeCombate naveAtacante naveAtacada = durabilidad (naveFueAtacada naveAtacante naveAtacada) == 0

navesQueQuedarianFueraDeCombate :: Nave -> Estrategia
navesQueQuedarianFueraDeCombate nave flota = filter (naveQueQuedarianFueraDeCombate nave) flota


venganzaNula :: Nave -> Bool
venganzaNula nave = ataque nave < 10

venganzaNulaDeUnaFlota :: Estrategia
venganzaNulaDeUnaFlota flota = filter venganzaNula flota


comoQuedaFlotaEnemiga :: Estrategia -> Nave -> Flota -> Flota
comoQuedaFlotaEnemiga estrategia nave flota = map (naveFueAtacada nave) (misionSorpresa estrategia flota)

misionSorpresa :: Estrategia -> Flota -> Flota
misionSorpresa estrategia flota = estrategia flota

--6--
bajaDurabilidadTotal :: Estrategia -> Estrategia -> Nave -> Flota -> Estrategia
bajaDurabilidadTotal estrategia1 estrategia2 nave flota
 | durabilidadTotal (comoQuedaFlotaEnemiga estrategia1 nave flota) < durabilidadTotal (comoQuedaFlotaEnemiga estrategia2 nave flota) = estrategia1
 | otherwise = estrategia2

llevarAdelanteMision :: Estrategia -> Estrategia -> Nave -> Flota -> Flota
llevarAdelanteMision estrategia1 estrategia2 nave flota = misionSorpresa (bajaDurabilidadTotal estrategia1 estrategia2 nave flota) flota

--7--
flotaInfinita :: Nave -> Flota
flotaInfinita nave = nave:flotaInfinita nave

-- no es posible determinar su durabilidad total ya que nunca terminaria de sumar la durabilidad de todas las naves (al ser infinito)
-- cuando se llega a cabo una mision sobre ella, nos tira error, ya que intenta devolver una flota infinita tambien
-}

{-------------------------------------------PARCIAL ESCUELITA DE THANOS-----------------------------------------
--1--
data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
} deriving (Show, Eq)

data Personaje = Personaje {
    nombre :: String,
    edad :: Number,
    planeta :: Planeta,
    energia :: Number,
    habilidades :: [Habilidad]
} deriving (Show, Eq)

type Habilidad = String
type Planeta = String

data Universo = Universo {
  personajes :: [Personaje]  
} deriving (Show,Eq)


guanteleteCompleto :: Guantelete
guanteleteCompleto = Guantelete "uru" [laMente 1, (elAlma "telequinesis"), (elEspacio "tierra"), elPoder , elTiempo, (gemaLoca elPoder)]


ironMan :: Personaje
ironMan = Personaje "Iron Man" 50 "Tierra" 10 ["ingenieria", "tecnologia"]

drStrange :: Personaje
drStrange = Personaje "Doctor Strange" 40 "Luna" 6 ["telequinesis", "telepatia"]

groot :: Personaje
groot = Personaje "This is Groot" 28 "Venus" 5 ["buena onda"]

wolverine :: Personaje
wolverine = Personaje "Wolverine" 42 "Tierra" 8 ["garras filosas", "trepar arboles"]

viudaNegra :: Personaje
viudaNegra = Personaje "Viuda Negra" 30 "Luna" 7 ["poder aracnido", "telequinesis"]


universo1 :: Universo
universo1 = Universo [ironMan, drStrange, groot, wolverine]

universo2 :: Universo
universo2 = Universo [ironMan, drStrange, groot, wolverine, viudaNegra]


chasquidoDeUnUniverso :: Guantelete -> Universo -> Universo
chasquidoDeUnUniverso guantelete universo
 | length (gemas guantelete) == 6 = universo {personajes = take ((personajesDeUnUniverso universo) `div` 2) (personajes universo)}
 | otherwise = universo

personajesDeUnUniverso :: Universo -> Number
personajesDeUnUniverso universo = length (personajes universo)

--2--
aptoParaPendex :: Universo -> Bool
aptoParaPendex universo = any esPendex (personajes universo)

esPendex :: Personaje -> Bool
esPendex personaje = (edad personaje) < 45


energiaTotal :: Universo -> Number
energiaTotal universo = sum (map energia (personajesConMasDeUnaHabilidad universo))

personajesConMasDeUnaHabilidad :: Universo -> [Personaje]
personajesConMasDeUnaHabilidad universo = filter tieneMasDeUnaHabilidad (personajes universo)

tieneMasDeUnaHabilidad :: Personaje -> Bool
tieneMasDeUnaHabilidad personaje = length (habilidades personaje) > 1

--3--
type Gema = Personaje -> Personaje

laMente :: Number -> Gema
laMente n personaje = personaje {energia = energia personaje - n}


elAlma :: Habilidad -> Gema
elAlma habilidad personaje = personaje {habilidades = eliminarHabilidad habilidad (habilidades personaje), energia = energia personaje - 10}

eliminarHabilidad :: Habilidad -> [Habilidad] -> [Habilidad]
eliminarHabilidad habilidad (habilidad1:restoDeHabilidades)
 | habilidad == habilidad1 = restoDeHabilidades
 | otherwise = habilidad1 : eliminarHabilidad habilidad restoDeHabilidades


elEspacio :: Planeta -> Gema
elEspacio planetaNuevo personaje = personaje {planeta = planetaNuevo, energia = energia personaje - 20}


elPoder :: Gema
elPoder personaje 
 | tiene2HabilidadesOMenos personaje = personaje {energia = 0, habilidades = []}
 | otherwise = personaje {energia = 0}

tiene2HabilidadesOMenos :: Personaje -> Bool
tiene2HabilidadesOMenos personaje = length (habilidades personaje) <= 2


elTiempo :: Gema
elTiempo personaje = personaje {energia = energia personaje - 50, edad = mitadNumeroMayorA18 (edad personaje)}

mitadNumeroMayorA18 :: Number -> Number
mitadNumeroMayorA18 n
 | n <= 36 = 18
 | n > 36 = n `div` 2


gemaLoca :: Gema -> Gema
gemaLoca gema personaje = gema(gema personaje)

--4--
guanteleteDeGoma :: Guantelete
guanteleteDeGoma = Guantelete "Goma" [elTiempo, elAlma "usar Mjolnir", gemaLoca (elAlma "programacion en Haskell")]

--5--
utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas personaje = foldl (\personaje gema -> usarGema gema personaje) personaje gemas
--debemos aplicar lambda porque la funcion foldl acepta funciones de tipo (b -> a -> b), mientras que usarGema es del tipo (b -> a -> a)

utilizarr :: [Gema] -> Personaje -> Personaje
utilizarr gemas personaje = foldr usarGema personaje gemas
--funciona con foldr ya que la funcion "usarGema" es de tipo (b -> a -> a), donde a es el acumulador y b un elemento de la lista

usarGema :: Gema -> Personaje -> Personaje
usarGema gema personaje = gema personaje

--6--
gemaMasPoderosaDeUnaListaDeGemas :: [Gema] -> Personaje -> Gema
gemaMasPoderosaDeUnaListaDeGemas [gema] _ = gema
gemaMasPoderosaDeUnaListaDeGemas gemas personaje = foldl (\gema1 gema2 -> esMasPoderosa gema1 gema2 personaje) (head gemas) (tail gemas)

esMasPoderosa :: Gema -> Gema -> Personaje -> Gema
esMasPoderosa gema1 gema2 personaje
 | energia (gema1 personaje) < energia (gema2 personaje) = gema1
 | otherwise = gema2

--usando recursividad:
gemaMasPoderosaDeUnaListaDeGemas' :: [Gema] -> Personaje -> Gema
gemaMasPoderosaDeUnaListaDeGemas' [gema] _ = gema
gemaMasPoderosaDeUnaListaDeGemas' (gema1:gema2:restoDeGemas) personaje
    | energia (gema1 personaje) < energia (gema2 personaje) = gemaMasPoderosaDeUnaListaDeGemas' (gema1:restoDeGemas) personaje
    | otherwise = gemaMasPoderosaDeUnaListaDeGemas' (gema2:restoDeGemas) personaje

--7--
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas elTiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

-- "gemaMasPoderosa wolverine guanteleteDeLocos" = no se puede, sea quien sea el personaje ya que nunca terminaria de comparar las gemas (al ser infinitas).
-- "usoLasTresPrimerasGemas guanteleteDeLocos punisher" = sí se puede ejecutar, ya que gracias a la lazy evaluation, haskell toma las primeras 3 gemas que pide la funcion "usoLasTresPrimerasGemas" y deja de ejecutar la lista infinita, dando un resultado para los elementos tomados.
-}

{--------------------------------------------PARCIAL VACACIONES----------------------------------------
data Turista = Turista {
    cansancio :: Number,
    stress :: Number,
    estaViajandoSolo :: Bool,
    idiomas :: [String]
} deriving (Show, Eq)

type Excursion = Turista -> Turista


irALaPlaya :: Excursion
irALaPlaya personaje
 | estaViajandoSolo personaje = personaje {cansancio = cansancio personaje - 5}
 | otherwise = personaje {stress = stress personaje - 1}

apreciarElementoDelPaisaje :: String -> Excursion
apreciarElementoDelPaisaje elemento turista = turista {stress = stress turista - (length elemento)}

hablarIdioma :: String -> Excursion
hablarIdioma idioma turista = turista {idiomas = idiomas turista ++ [idioma], estaViajandoSolo = False}

caminarNMinutos :: Number -> Excursion
caminarNMinutos n turista = turista {cansancio = cansancio turista + (n/4), stress = stress turista - (n/4)}

paseoEnBarco :: Marea -> Excursion
paseoEnBarco marea turista
 | marea == Fuerte = turista {stress = stress turista + 6, cansancio = cansancio turista + 10}
 | marea == Moderada = turista
 | marea == Tranquila = hablarIdioma "Aleman" (apreciarElementoDelPaisaje "mar" (caminarNMinutos 10 turista))

data Marea = Fuerte | Moderada | Tranquila deriving (Show, Eq)

--2--
ana :: Turista
ana = Turista 0 21 True ["español"]

beto :: Turista
beto = Turista 15 15 False ["aleman"]

cathi :: Turista
cathi = Turista 15 15 False ["aleman"]

joako :: Turista
joako = Turista 5 1 False ["español", "ingles", "portugues","japones(???"]

cami :: Turista
cami = Turista 5 2 False ["español", "inglés"]

milo :: Turista
milo = Turista 0 0 True ["gatuno"]

--3--
turistaHaceExcursion :: Excursion -> Turista -> Turista
turistaHaceExcursion excursion turista = baja10percentDeStress (excursion (turista))

baja10percentDeStress :: Turista -> Turista
baja10percentDeStress turista = turista {stress = stress turista - (stress turista * 0.1)}


deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Number) -> Turista -> Excursion -> Number
deltaExcursionSegun f turista excursion = (f (turista)) - (f (excursion (turista)))


cansancioDeTurista :: Turista -> Number
cansancioDeTurista turista = cansancio turista

stressDeUnTurista :: Turista -> Number
stressDeUnTurista turista = stress turista

cantidadDeIdiomasTurista :: Turista -> Number
cantidadDeIdiomasTurista turista = length (idiomas turista)


esEducativa :: Turista -> Excursion -> Bool
esEducativa turista excursion = deltaExcursionSegun cantidadDeIdiomasTurista turista excursion >= 1

excursionesMasDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesMasDesestresantes turista excursiones = filter (reduceAlMenos3DeStress turista) excursiones

reduceAlMenos3DeStress :: Turista -> Excursion -> Bool
reduceAlMenos3DeStress turista excursion = (stress (turista) - stress (excursion(turista))) <= 3


esMasDesesestresante :: Turista -> Excursion -> Excursion -> Excursion
esMasDesesestresante turista e1 e2
 | stress (e1 (turista)) < stress (e2 (turista)) = e1
 | otherwise = e2

excursionMasDesestresante :: Turista -> [Excursion] -> Excursion
excursionMasDesestresante _ [excursion] = excursion
excursionMasDesestresante turista (e1:e2:excursiones) 
 | stress (e1 (turista)) < stress (e2 (turista)) = excursionMasDesestresante turista (e1:excursiones)
 | otherwise = excursionMasDesestresante turista (e2:excursiones)

--4--
type Tour = [Excursion]

tourCompleto :: Tour
tourCompleto = [hablarIdioma "malmacquiano", caminarNMinutos 40, apreciarElementoDelPaisaje "cascada", caminarNMinutos 20]

ladoB :: Excursion -> Tour
ladoB excursion = [caminarNMinutos 120, excursion, paseoEnBarco Tranquila]

islaVecina :: Marea -> Excursion -> Tour
islaVecina marea excursion
 | marea == Fuerte = [paseoEnBarco Fuerte, excursion, apreciarElementoDelPaisaje "lago", paseoEnBarco Fuerte]
 | otherwise = [paseoEnBarco marea, excursion, irALaPlaya, paseoEnBarco marea]

-- un turista realiza el tour completo
tourCompleto' :: Turista -> Turista
tourCompleto' = (hablarIdioma "malacquiano") . (caminarNMinutos 40) . (apreciarElementoDelPaisaje "cascada") . (caminarNMinutos 20 )


haceTour :: Tour -> Turista -> Turista
haceTour excursiones turista = foldr turistaHaceExcursion (aumentaStressSegunTour excursiones turista) excursiones
-- foldr ya que la funcion turistaHaceExcursion es de tipo (b -> a -> a) donde a es el acumulador

cuantoStressAumentaSegunTour :: Tour -> Number
cuantoStressAumentaSegunTour excursiones = length excursiones

aumentaStressSegunTour :: Tour -> Turista -> Turista
aumentaStressSegunTour excursiones turista = turista {stress = stress turista + (cuantoStressAumentaSegunTour excursiones)}


excursionDesestresante :: Turista -> Excursion -> Bool
excursionDesestresante turista excursion = stress (excursion(turista)) < stress turista

excursionesDesestresantes :: Turista -> Tour -> [Excursion]
excursionesDesestresantes turista tour = filter (excursionDesestresante turista) tour

dejaAlTuristaAcompañado ::Turista -> Excursion -> Bool
dejaAlTuristaAcompañado turista excursion
 | estaViajandoSolo (excursion turista) == False = True
 | otherwise = False

tourConvincente :: Turista -> Tour -> Bool
tourConvincente turista tour = any (dejaAlTuristaAcompañado turista) (excursionesDesestresantes turista tour)

existeTourConvincente :: Turista -> [Tour] -> Bool
existeTourConvincente turista tours = any (tourConvincente turista) tours

toursConvincentes :: Turista -> [Tour] -> [Tour]
toursConvincentes turista tours = filter (tourConvincente turista) tours



efectividad :: Tour -> [Turista] -> Number
efectividad tour = sum . map (espiritualidadAportada tour) . filter (flip tourConvincente tour)

espiritualidadAportada :: Tour -> Turista -> Number
espiritualidadAportada tour = negate . deltaRutina tour

deltaRutina :: Tour -> Turista -> Number
deltaRutina tour turista = deltaSegun nivelDeRutina (haceTour tour turista) turista

nivelDeRutina :: Turista -> Number
nivelDeRutina turista = cansancio turista + stress turista

--5--
tourConInfPlayas :: Excursion -> Tour
tourConInfPlayas excursion = irALaPlaya : (tourConInfPlayas irALaPlaya)

excursionNumeroN :: Number -> Tour -> Excursion
excursionNumeroN n excursiones = excursiones !! n
--}