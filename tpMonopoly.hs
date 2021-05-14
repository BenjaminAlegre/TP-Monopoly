import Text.Show.Functions()

data Jugador = UnJugador {
    nombre :: Nombre,
    dinero :: Dinero,
    tactica :: Tactica,
    propiedades :: [Propiedad],
    acciones :: [Accion]
} deriving Show

data Propiedad = UnaPropiedad {
    nombre :: Nombre,
    dinero :: Dinero
} deriving Show

type Accion = Jugador -> Jugador
type Nombre = String
type Dinero = Int
type Tactica = String

-- Participantes --

carolina :: Jugador
carolina = UnJugador "Carolina" 500 ["Accionista"] [] [pasarPorBanco, pagarAAccionista]

manuel :: Jugador
manuel = UnJugador "Manuel" 500 ["Oferente singular"] [] [pasarPorBanco, enojarse]

--Funciones --

pasarPorElBanco :: Accion
pasarPorElBanco unJugador = unJugador { dinero = cambiarDinero 40 unJugador , tactica = "Comprador compulsivo"}

enojarse :: Accion
enojarse unJugador = unJugador { dinero = cambiarDinero 50 unJugador, acciones= (++) (acciones unJugador) ["gritar"]  }

gritar :: Accion
gritar unJugador = unJugador { nombre = (++) "AHHHH" nombre unJugador}

subastar :: Propiedad -> Accion 
subastar unaPropiedad unJugador
    | tieneTacticas unJugador = comprarUnaPropiedad unaPropiedad unJugador
    | otherwise

cobrarAlquileres :: Accion
cobrarAlquileres unJugador = unJugador { dinero = cambiarDinero (alquilerPropiedades unJugador) unJugador}

pagarAAccionistas :: Accion
pagarAAccionistas unJugador 
    | tieneUnaTactica unJugador "Accionista" = unJugador { dinero = cambiarDinero 200 (dinero unJugador)
    | otherwise = unJugador { dinero = cambiarDinero (-100) (dinero unJugador)}

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor unaPropiedad unJugador
    | tieneDineroParaComprar unaPropiedad unJugador = comprarUnaPropiedad unaPropiedad unJugador
    | otherwise = berrinche unaPropiedad unJugador

ultimaRonda :: Jugador -> Accion
ultimaRonda unJugador = foldl1 (.) $ acciones unJugador

juegoFinal :: Jugador -> Jugador -> Jugador
juegoFinal unJugador otroJugador 
    | jugadorGanador unJugador otroJugador = unJugador
    | otherwise = otroJugador

-- Funciones Auxiliares --

cambiarDinero ::  Dinero -> Jugador -> Dinero    
cambiarDinero ganancia unJugador = (+) (dinero unJugador) ganancia 

cambiarTactica :: String -> Jugador -> 
cambiarTactica unaTactica unJugador = unJugador {tactica = unaTactica }

tieneUnaTactica :: Jugador -> Tactica -> Bool
tieneUnaTactica unJugador unaTactica = unaTactica == tactica unJugador

tieneTacticas :: Jugador -> Bool
tieneTacticas unJugador = tieneUnaTactica unJugador "Oferente Singular" || tieneUnaTactica unJugador "Accionista"

comprarUnaPropiedad :: Propiedad -> Accion
comprarUnaPropiedad unaPropiedad unJugador = unJugador { dinero = (-) (dinero unJugador) (dinero unaPropiedad), propiedad = (++) (propiedad unJugador) ( nombre unaPropiedad) }

alquilerDePropiedades :: Jugador -> Int
alquilerDePropiedades unJugador = (+) ((*10).length.filter (<150) $ (propiedades unJugador)) ((*20).length.filter (>150) $ (propiedades unJugador))

tieneDineroParaComprar :: Propiedad -> Jugador -> Bool
tieneDineroParaComprar unaPropiedad unJugador = (>=) (dinero unJugador) (dinero unaPropiedad)

dineroFaltante :: Propiedad -> Jugador -> Bool
dineroFaltante unaPropiedad unJugador = (*10).div $ ((dinero unaPropiedad) - (dinero unJugador) 10)

berrinche :: Propiedad -> Accion
berrinche unaPropiedad unJugador = unJugador { dinero = cambiarDinero 10 (dinero unJugador) , acciones = acciones unJugador ++ ["gritar"] }

jugadorGanador :: Jugador -> Jugador -> Bool
jugadorGanador unJugador otroJugador = (>) (dinero unJugador) (dinero otroJugador)