import Data.Char
import Control.Concurrent--borrar esto
import Data.Typeable
import Data.List
---------------------------
--https://stackoverflow.com/questions/22166912/how-to-close-a-file-in-haskell
--Necesitaba poder escribir al archivo
--evaluate(force file) solucionó el problema
---------------------------
import Control.DeepSeq-----
import Control.Exception---
---------------------------

import Estructuras
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
infoComercial = crearEmpresa(["nombreEmpresa", "webEmpresa","12345", "2234.2", "55.3"])

---------------------------------------------------------------------------------------
--menuPrincipal :: Integer -> [Parqueo] -> IO()
menuPrincipal (opcion, lParqueos, lBicicletas, lUsuarios) =
    if opcion == 3 then
        print ("salir")
    else
        do
            case opcion of
                -1-> putStr("")
                1-> menuOperativo (-1,lParqueos, lBicicletas, lUsuarios)
                2-> menuGeneral (-1,lParqueos, lBicicletas, lUsuarios)

            putStrLn("\n\nMenú principal")
            print("1.Opciones operativas")
            print("2.Opciones administrativas")
            print("3.Salir")
            putStrLn "Indique la opción: "
            tempOpcion <- getLine
            let opcion = (read tempOpcion :: Integer)
            menuPrincipal(opcion, lParqueos, lBicicletas, lUsuarios)

--menuOperativo :: Integer -> [Parqueo] -> IO()
menuOperativo (opcion, lParqueos, lBicicletas, lUsuarios) =
    if opcion == 5 then
        print ("volviendo")
    else
        do
            case opcion of
                -1-> putStrLn ("")
                1-> do
                    parqueos <-cargarParqueos lParqueos
                    showParqueos parqueos
                2-> do
                    mostrarBicicletas lBicicletas
                    
                3-> do
                    usuarios <- cargarUsuarios lUsuarios
                    putStrLn("Se han cargado los siguiente usuarios")
                    showUsuarios usuarios
                4-> do
                    menuEstadisticas(-1, lParqueos, lBicicletas, lUsuarios)

            putStr("\nMenú operativo")
            putStr("\n1. Mostrar parqueos")
            putStr("\n2. Mostrar bicicletas")
            putStr("\n3. Cargar usuarios")
            putStr("\n4. Estadisticas")
            putStr("\n5. Volver")
            putStrLn "\nIndique la opción: "
            tempOpcion <- getLine
            let opcion = (read tempOpcion :: Integer)
            menuOperativo(opcion, lParqueos, lBicicletas, lUsuarios)

mostrarBicicletas lBicicletas= do
    bicicletas <-cargarBicicletas lBicicletas
    putStrLn "Ingrese el nombre del parqueo que desea consultar \
    \\n# Para consultar todas las bicicletas\
    \\no transito para consultar las bicicletas en transito\nOpción: "
    parqueo <- getLine
    if parqueo == "transito" then
        showBicicletas (bicicletas,"en transito")
    else
        showBicicletas (bicicletas,parqueo)


menuGeneral (opcion, lParqueos, lBicicletas, lUsuarios) =
    if opcion == 4 then
        print ("volviendo")
    else
        do
            parqueos <-cargarParqueos lParqueos
            bicicletas <-cargarBicicletas lBicicletas
            case opcion of
                -1-> putStrLn ("")
                1-> do
                    putStrLn("1. Consultar bicicletas") 
                    consultarBicicletas parqueos
                2-> do
                    putStrLn("2. Alquilar")  
                    alquilar( parqueos, bicicletas, lUsuarios)
                3-> do
                    putStrLn("3. Facturar")
                    facturar (bicicletas,parqueos)

            putStrLn("\nMenú operativo")
            putStrLn("1.Consultar bicicletas")
            putStrLn("2.Alquilar")
            putStrLn("3.Facturar")
            putStrLn("4.Volver")
            putStrLn "Indique la opción: "
            tempOpcion <- getLine
            let opcion = (read tempOpcion :: Integer)
            menuGeneral(opcion, lParqueos, lBicicletas, lUsuarios)
-----------------------------------------------------------
--------------------------------------------------------

menuEstadisticas (opcion, lParqueos, lBicicletas, lUsuarios) =
    if opcion == 5 then
        print ("volviendo")
    else
        do
            lFacturas <- cargarFacturas "facturas.txt"
            case opcion of
                -1-> putStrLn ("")
                1-> do
                    putStrLn("top 5 usuarios")
                    getTop5Usuarios lFacturas
                2-> do
                    putStrLn("top 5 parqueos")
                   -- getTop5Parqueos lFacturas
                3-> do
                    putStrLn("top 3 bicicletas")
                4-> putStrLn("resumen")

            putStr("\nMenú estadisticas")
            putStr("\n1. Top 5 usuarios con más viajes")
            putStr("\n2. Top 5 parqueos con más viajes")
            putStr("\n3. Top 3 bicicletas con más kilometros recorridos")
            putStr("\n4. Resumen")
            putStr("\n5. Volver")
            putStrLn "\nIndique la opción: "
            tempOpcion <- getLine
            let opcion = (read tempOpcion :: Integer)
            menuEstadisticas (opcion, lParqueos, lBicicletas, lUsuarios)

--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
getTop5Usuarios lFacturas= do
    
    let lViajesUsuario = elimViajesRepetidos(getViajesXUsuario(lFacturas))
    let ordenada = sortBy(\x1 x2 -> compare (last x2) (last x1)) lViajesUsuario --https://stackoverflow.com/questions/19587518/ordering-a-list-of-lists-in-haskell
    let top5 = take 5 ordenada
    imprimirListaTop (top5)

getViajesXUsuario lFacturas = do
    if lFacturas == [] then
        []
    else do
        let elemento = head lFacturas
        let usuario = getUsuarioFactura(elemento)
        let cantViajes = getViajesXUsuarioAux(usuario, lFacturas)
        --putStrLn ("usuario: " ++ show usuario ++ " cantidad: " ++ show cantViajes)
        [[show usuario]++[show cantViajes]] ++ getViajesXUsuario(tail lFacturas)
        



getViajesXUsuarioAux (usuario, lFacturas) = do
    if lFacturas == [] then
        0
    else do
        let elemento = head lFacturas
        let usuarioTemp = getUsuarioFactura(elemento)
        if usuarioTemp == usuario then
            1 + getViajesXUsuarioAux(usuario, tail lFacturas)
        else
            getViajesXUsuarioAux(usuario, tail lFacturas)


imprimirListaTop lista = do
    --print lista
    if lista == [] then
        return ()
    else do 
        let primero = head lista
        putStrLn ("Usuario: " ++ head primero ++ " cantidad: " ++last primero)
        imprimirListaTop (tail lista)

elimViajesRepetidos (lViajesUsuario) = do
    if length lViajesUsuario == 0 then
        []
    else do
        let primero = head lViajesUsuario
        let usuario = head primero
        let nuevaLista = filter(\x ->(head x) /= usuario) lViajesUsuario
        [primero] ++ elimViajesRepetidos(nuevaLista)

----------------------------------------------------------------------
--------------------------------------------------------------------------
facturar(bicicletas,lParqueos) = do
    putStrLn("facturar alquiler")
    listaAlquileres <-cargarAlquileres "alquileres.txt"
    alquilerTemp <- seleccionarAlquiler listaAlquileres
    if alquilerTemp == "#" then 
        print " Se ha cancelado la operación"
    else do
        --let bicicleta = getBicicletaAlquilada (read alquilerTemp::Integer, bicicletas, listaAlquileres)
        lFacturas <- cargarFacturas "facturas.txt"
        alquilerInfo <- getAlquiler (read alquilerTemp::Integer, listaAlquileres)
        let cantFacturas = length lFacturas
        let tipoBicicleta = getTipoBicicleta2((getIdBicicletaAlquiler(alquilerInfo)),bicicletas)
        let tarifaKm =getTarifa(tipoBicicleta)
        let pSalida = getSalidaAlquiler(alquilerInfo)
        let pLlegada = getLlegadaAlquiler(alquilerInfo)
        let idBicicleta =getIdBicicletaAlquiler(alquilerInfo)
        distaciaRecorrida <- getDistaciaRecorrida(pSalida, pLlegada, lParqueos)

        let factura = crearFactura([show cantFacturas,show(getCedulaAlquiler(alquilerInfo)), pSalida, pLlegada,idBicicleta, tipoBicicleta, show distaciaRecorrida,show tarifaKm, show (distaciaRecorrida * tarifaKm)])
        appendFile "facturas.txt" (show (getCodigoFactura(factura))++","++show(getUsuarioFactura(factura))++","++getPSalidaFact(factura)++","++getPLlegadaFact(factura)++","++getBiciFactura(factura)++","++getTipoBiciFactura(factura)++","++show(getCantKM(factura))++","++show(getTarifaKMFactura(factura))++","++show(getTotalFactura(factura))++"\n")
        bicicletaUbicacion(bicicletas, idBicicleta, pLlegada)
        facturarAlquiler(listaAlquileres, getCodigoAlquiler(alquilerInfo), "facturado")

        
        
        printFactura(factura)

printFactura(factura) = do
       putStrLn ("Codigo: " ++show (getCodigoFactura(factura)) ++ "\
       \ \nNombre empresa: " ++ getNombreEmpresa(infoComercial)++"\
       \ \nSitio web: " ++ getWebEmpresa(infoComercial) ++ "\
       \ \nContacto: " ++ show (getContactoEmpresa(infoComercial)) ++ "\
       \ \nUsuario: " ++ show (getUsuarioFactura(factura)) ++ "\
       \ \nParqueo Salida: " ++ getPSalidaFact(factura) ++ "\
       \ \nparqueo Llegada: " ++ getPLlegadaFact(factura) ++ "\
       \ \nidBicicleta: "++ getBiciFactura(factura)++ "\
       \ \ntipoBicicleta: "++ (if getTipoBiciFactura(factura) =="Ae" then "asistencia electrica" else "pedales") ++ "\
       \ \nDistancia recorrida: " ++ show (getCantKM(factura)) ++"\
       \ \nTarifa x Kilometro: " ++ show (getTarifaKMFactura(factura))++ "\
       \ \nTotal : " ++ show ( getTotalFactura(factura)))

getDistaciaRecorrida(pSalida, pLlegada, lParqueos) = do
    parqueoSalida <- getParqueoXNombre(pSalida,lParqueos)
    parqueoLlegada <- getParqueoXNombre(pLlegada, lParqueos)
    let 
        x1 = getUbicacionX(parqueoSalida)
        x2 = getUbicacionX(parqueoLlegada)
        y1 = getUbicacionY(parqueoSalida)
        y2 = getUbicacionY(parqueoLlegada)
        f =calcularDistancia(x1,x2,y1,y2) 
    return f

getParqueoXNombre(nombre, lParqueos) = do
    let primero = head lParqueos
    let nombreTemp = getNombreParqueo(primero)
    if nombre == nombreTemp then
        return primero
    else
        getParqueoXNombre(nombre, tail lParqueos)

getTarifa(tipoTarifa) = do
    if tipoTarifa == "AE" then
        getTarKmEle(infoComercial)
    else 
        getTarKmPedal(infoComercial)


getTipoBicicleta2(idBicicleta, bicicletas) = do
    let primero = head bicicletas
    let tipo = getTipoBicicleta(primero)
    let idTemp = getIdBicicleta(primero)
    if idBicicleta == idTemp then
        tipo
    else do
        getTipoBicicleta2(idBicicleta, tail bicicletas)


getAlquiler(idAlquiler, lAlquileres) = do
    let primero = head lAlquileres
    let idTemp = getCodigoAlquiler(primero)
    if idAlquiler == idTemp then
         return primero
    else do
        getAlquiler(idAlquiler, tail lAlquileres)


seleccionarAlquiler lAlquileres = do
    putStrLn "Ingrese el identificador del alquiler o (#) para cancelar la facturación: "
    alquiler <- getLine
    if alquiler == "#" then 
        return alquiler
    else do
        if (all isDigit alquiler) && existeAlquiler(lAlquileres, read alquiler::Integer,"activo" ) then
            
            return alquiler
        else do
            putStrLn "Este alquiler no existe o ya fue facturado"
            seleccionarAlquiler lAlquileres


existeAlquiler ([], alquiler, estado) = False
existeAlquiler(lAlquileres, alquiler, estado)= do
    let primero = (head lAlquileres)
    let idTemp = getCodigoAlquiler(primero)
    let estadoTemp = getEstadoAlquiler(primero)
    if alquiler == idTemp then
        if estado == estadoTemp then
            True
        else
            False
    else
        existeAlquiler((tail lAlquileres), alquiler,estado)

---------------------------------------------------------------
--------------------------------------------------------------
alquilar(lParqueos, lBicicletas, lUsuarios) = do
    putStrLn ("\n\nalquiler")
    usuarios <- cargarUsuarios lUsuarios
    showUsuarios usuarios
    usuario <- seleccionarUsuario usuarios
    if usuario == "#" then do
        print ("Se ha cancelado la operación")
    else do
        showParqueos lParqueos
        putStrLn "Seleccione el parqueo de salida"
        parqueoSalida <- seleccionarParqueoS (lParqueos,lBicicletas)
        if parqueoSalida == "#" then do
            print ("Se ha cancelado la operación")
        else do
            parqueoLlegada <- seleccionarParqueoL (lParqueos, parqueoSalida)
            if parqueoLlegada == "#" then do
                print ("Se ha cancelado la operación")
            else do
                let bicicletasParqueo =getBicicletasParqueo(lBicicletas, parqueoSalida)
                showBicicletas(lBicicletas,parqueoSalida)
                bicicleta <- seleccionarBicicleta(bicicletasParqueo)
                if bicicleta == "#" then do
                    print ("Se ha cancelado la operación")
                else do
                    lAlquileres <- cargarAlquileres "alquileres.txt"
                    let cantAlquileres = length lAlquileres
                    appendFile "alquileres.txt" (show cantAlquileres ++","++usuario++",\
                                                \"++parqueoSalida++","++parqueoLlegada++",\
                                                \"++bicicleta++",activo\n")
                    bicicletaUbicacion(lBicicletas,bicicleta,"en transito")                            
                    putStrLn( "Codigo: " ++ show cantAlquileres ++"\ 
                                \ \nCedula: " ++ usuario ++"\
                                \ \nSalida: " ++ parqueoSalida ++ "\
                                \ \nLlegada: "++ parqueoLlegada++ "\
                                \ \nBicicleta: "++ bicicleta)

        
-------------------------------------------------------------------------------------------
facturarAlquiler(lAlquileres,alquiler, estado) = do
    writeFile "alquileres.txt" ""
    facturarAlquilerAux(lAlquileres, alquiler, estado)

facturarAlquilerAux(lAlquileres, alquiler, estado) = do
    if lAlquileres == [] then
        return ()
    else do
        let primero = head lAlquileres
        let codigoTemp = getCodigoAlquiler(primero)
        let cedulaTemp = getCedulaAlquiler(primero)
        let pSalidaTemp = getSalidaAlquiler(primero)
        let pLlegadaTemp = getLlegadaAlquiler(primero)
        let idBicicletaTemp = getIdBicicletaAlquiler(primero)
        let estadoTemp = getEstadoAlquiler(primero)
        if alquiler == codigoTemp then do
            appendFile "alquileres.txt" (show codigoTemp ++ "," ++ show cedulaTemp ++ "," ++ pSalidaTemp ++ "," ++ pLlegadaTemp ++ "," ++ idBicicletaTemp  ++ "," ++ estado ++ "\n")
            facturarAlquilerAux(tail lAlquileres,alquiler, estado)
        else do
            appendFile "alquileres.txt" (show codigoTemp ++ "," ++ show cedulaTemp ++ "," ++ pSalidaTemp ++ "," ++ pLlegadaTemp ++ "," ++ idBicicletaTemp  ++ "," ++ estadoTemp ++ "\n")
            facturarAlquilerAux(tail lAlquileres,alquiler, estado)

bicicletaUbicacion(lBicicletas,bicicleta, ubicacion)= do
    writeFile "bicicletas.txt" ""
    bicicletaUbicacionAux (lBicicletas,bicicleta, ubicacion)

bicicletaUbicacionAux(lBicicletas, bicicleta, ubicacion) = do
    if lBicicletas == [] then 
        return ()
    else do
        let primero = (head lBicicletas)
        let idTemp = getIdBicicleta(primero)
        let tipoTemp = getTipoBicicleta(primero)
        let parqueoTemp = getParqueoBicicleta(primero)
        if bicicleta == idTemp then do
            appendFile "bicicletas.txt" (idTemp++","++tipoTemp++","++ubicacion ++"\n")
            bicicletaUbicacionAux(tail lBicicletas, bicicleta, ubicacion)
        else do
            appendFile "bicicletas.txt" (idTemp++","++tipoTemp++","++parqueoTemp++"\n")
            bicicletaUbicacionAux(tail lBicicletas, bicicleta, ubicacion)


---------------------------------------------------------------------------------
getBicicletasParqueo (lBicicletas, nombreParqueo) = do
    if lBicicletas == [] then 
        []
    else do
        let elemento = head lBicicletas
        if getParqueoBicicleta(elemento) == nombreParqueo then
            [elemento] ++ getBicicletasParqueo(tail lBicicletas, nombreParqueo)
        else
            getBicicletasParqueo(tail lBicicletas, nombreParqueo)


seleccionarBicicleta lBicicletas = do
    putStrLn "Ingrese el identificador de la bicicleta o (#) para cancelar el alquiler: "
    bicicleta <- getLine
    if bicicleta == "#" then 
        return bicicleta
    else do
        if existeBicicleta(lBicicletas, bicicleta) then
            return bicicleta
        else do
            putStrLn "Esta bicicleta no existe o no se encuetra disponible"
            seleccionarBicicleta lBicicletas


existeBicicleta ([], idBicicleta) = False
existeBicicleta (lBicicletas, idBicicleta)= do
    let primero = (head lBicicletas)
    let idTemp = getIdBicicleta(primero)
    if idBicicleta == idTemp then
        True
    else
        existeBicicleta((tail lBicicletas), idBicicleta)

 ------------------------------------------------------------------------------------------   
 ------------------------------------------------------------------------------------------   


seleccionarParqueoS (lParqueos, lBicicletas) = do
    putStrLn "Ingrese el nombre del parqueo de salida o (#) para cancelar el alquiler: "
    parqueoSalida <- getLine
    if parqueoSalida == "#" then 
        return parqueoSalida
    else do
        if existeParqueo(lParqueos, parqueoSalida) then do
            let bicicletasParqueo =getBicicletasParqueo(lBicicletas, parqueoSalida)
            if length bicicletasParqueo == 0 then do
                putStrLn "En este momento no hay bicicletas disponibles en este parqueo"
                seleccionarParqueoS (lParqueos, lBicicletas)
            else
                return parqueoSalida
        else do
            putStrLn "El parqueo no existe"
            seleccionarParqueoS (lParqueos, lBicicletas)


seleccionarParqueoL (lParqueos, pSalida)  = do
    putStrLn "Ingrese el nombre del parqueo de llegada o (#) para cancelar el alquiler: "
    parqueoLlegada <- getLine
    if parqueoLlegada == "#" then 
        return parqueoLlegada
    else do
        if existeParqueo(lParqueos, parqueoLlegada) then
            if (pSalida /= parqueoLlegada) then
                return parqueoLlegada
            else do
                putStrLn "El parqueo de llegada no puede ser el mismo que el de salida"
                seleccionarParqueoL (lParqueos, pSalida)
        else do
            putStrLn "El parqueo no existe"
            seleccionarParqueoL(lParqueos, pSalida)


existeParqueo ([], nombre) = False
existeParqueo (lParqueos, nombre)= do
    let primero = (head lParqueos)
    let nombreTemp = getNombreParqueo(primero)
    if nombre == nombreTemp then
        True
    else
        existeParqueo((tail lParqueos), nombre)




-------------------------------------------------------------------------
seleccionarUsuario lUsuarios = do
    putStrLn "Ingrese la cedula del usuario o (#) para cancelar el alquiler"
    cedulaUsuario <- getLine
    if cedulaUsuario == "#" then 
        return cedulaUsuario
    else do
        if (all isDigit cedulaUsuario) && existePersona(lUsuarios, read cedulaUsuario::Integer) then
            return cedulaUsuario
        else do
            putStrLn "El usuario no existe"
            seleccionarUsuario lUsuarios


existePersona ([], cedula) = False
existePersona (lUsuarios, cedula)= do
    let primero = (head lUsuarios)
    let cedulaTemp = getCedulaUsuario(primero)
    if cedula == cedulaTemp then
        True
    else
        existePersona((tail lUsuarios), cedula) 

-------------------------------------------------------------

--------------------------------------------------------------
consultarBicicletas lParqueos = do
    putStrLn "Indique x: "
    pX<- getLine
    putStrLn "Indique y: "
    pY<- getLine
    getParqueoCercano (read (pX) ::Float, read (pY) ::Float, lParqueos, (head lParqueos))


--getParqueoCercano :: (Integer, Integer, [Parqueo]) -> Parqueo

getParqueoCercano (x1, y1, lParqueos,cercano) = do
    if lParqueos == [] then do
        let 
            nombre =  getNombreParqueo(cercano)
            direccion = getDireccionParqueo(cercano)
            provincia = getProvinciaParqueo(cercano)
            x = getUbicacionX(cercano)
            y = getUbicacionY(cercano)
        putStrLn ("\n\n--------------------------------\
                    \ \nEl parqueo más cercano es: \
                    \ \n\nParqueo: " ++ show nombre ++ "\
                    \ \nDireccion: " ++ show direccion ++ "\
                    \ \nProvincia: " ++ show provincia ++ "\
                    \ \nX: " ++ show x ++ "\
                    \ \nY: " ++ show y ++ "\
                    \ \n-----------------------------------")
    else
        do
        let par = head lParqueos
        print (calcularDistanciaParqueo(x1,y1,par))
        if calcularDistanciaParqueo(x1,y1,par)<= calcularDistanciaParqueo(x1,y1,cercano) then
            getParqueoCercano(x1,y1,(tail lParqueos),par)
        else
            getParqueoCercano(x1,y1,(tail lParqueos),cercano)


calcularDistanciaParqueo(x1,y1, parqueo) =do 
    let x2 = getUbicacionX(parqueo)
    let y2 = getUbicacionY(parqueo)
    calcularDistancia(x1,x2,y1,y2)

calcularDistancia(x1,x2,y1,y2) = 

    sqrt((x2-x1)**2 + (y2-y1)**2)
------------------------------------------------

-----------------------------------------------
cargarParqueos :: FilePath-> IO[Parqueo] 
cargarParqueos archivo = do
        contenido <- readFile archivo
        let lParqueos = separaParqueos (toLines contenido)
        return lParqueos

separaParqueos :: [[Char]]-> [Parqueo]
separaParqueos (lista) = 
    if lista == [] then
        []
    else
        [crearParqueo(separarPorComas((head lista), ""))] ++ separaParqueos (tail lista)
-------------------------------------------------------------------------------------------

----------------------------------------------------------
toLines :: String -> [String]
toLines texto = lines texto

----------------------------------------------------
---------------------------------------------------
cargarAlquileres :: FilePath-> IO [Alquiler] 
cargarAlquileres archivo = do
        contenido <- readFile archivo
        let lAlquileres = separarAlquileres (toLines contenido)
        evaluate(force contenido)
        return lAlquileres

separarAlquileres :: [[Char]]  -> [Alquiler]        
separarAlquileres (lista) = 
    if lista == [] then
        []
    else
        [crearAlquiler(separarPorComas((head lista), ""))] ++ separarAlquileres (tail lista)
---------------------------------------------------------
-----------------------------------------------------------

cargarFacturas :: FilePath-> IO [Factura] 
cargarFacturas archivo = do
        contenido <- readFile archivo
        let lFacturas = separarFacturas (toLines contenido)
        evaluate(force contenido)
        return lFacturas

separarFacturas :: [[Char]]  -> [Factura]        
separarFacturas (lista) = 
    if lista == [] then
        []
    else
        [crearFactura(separarPorComas((head lista), ""))] ++ separarFacturas (tail lista)



-------------------------------------
----------------------------------------
cargarBicicletas :: FilePath-> IO [Bicicleta] 
cargarBicicletas archivo = do
        contenido <- readFile archivo
        let lBicicletas = separaBicicletas (toLines contenido)
        evaluate(force contenido)
        return lBicicletas

separaBicicletas :: [[Char]]  -> [Bicicleta]        
separaBicicletas (lista) = 
    if lista == [] then
        []
    else
        [crearBicicleta(separarPorComas((head lista), ""))] ++ separaBicicletas (tail lista)
----------------------------------------------------

----------------------------------------------------

cargarUsuarios :: FilePath-> IO [Usuario] 
cargarUsuarios archivo = do
        contenido <- readFile archivo
        let lUsuarios = separaUsuarios (toLines contenido)
        return lUsuarios

separaUsuarios :: [[Char]]  -> [Usuario]        
separaUsuarios (lista) = 
    if lista == [] then
        []
    else
        [crearUsuario(separarPorComas((head lista), ""))] ++ separaUsuarios (tail lista)

----------------------------------------------------

-----------------------------------------------------
separarPorComas :: ([Char], [Char]) -> [[Char]]
separarPorComas (cadena, temp) =
    if cadena == "" then [temp] else
        if (head cadena) == (head ",") then
            [temp] ++ separarPorComas((tail cadena), "")
            else
                separarPorComas ((tail cadena), temp++[(head cadena)])


--------------------------------------------------------------------------

--------------------------------------------------------------------------
showParqueo :: Parqueo -> [Char]
showParqueo parqueo =
    let 
        nombre = getNombreParqueo(parqueo)
        direccion =  getDireccionParqueo(parqueo)
        provincia = getProvinciaParqueo(parqueo)
        ubicacionX = getUbicacionX(parqueo)
        ubicacionY = getUbicacionY(parqueo)
    in
        "\nNombre: " ++ show nombre ++ "\tDireccion: " ++ show direccion ++ "\tProvincia: " ++ show provincia ++ "\tUbicacion x: "++ show ubicacionX ++"\tUbicacion y: " ++ show ubicacionY


showParqueos :: [Parqueo] -> IO()
showParqueos [] = print("")
showParqueos lista =
    do
        putStr(showParqueo (head lista))
        showParqueos(tail lista)

--------------------------------------------------------------------------

--------------------------------------------------------------------------

showBicicleta :: (Bicicleta,String) -> [Char]
showBicicleta (bicicleta,pUbicacion)=
    let 
        id = getIdBicicleta(bicicleta)
        tipo =  getTipoBicicleta(bicicleta)
        parqueo = getParqueoBicicleta(bicicleta)
    in
        if pUbicacion =="#" ||pUbicacion == parqueo then
            "\nIdentificador: " ++ show id ++ "\ttipo: " ++ show tipo ++ "\tParqueo: " ++ show parqueo
        else ""

showBicicletas :: ([Bicicleta],String) -> IO()
showBicicletas ([],pUbicacion) = print("")
showBicicletas (lista ,pUbicacion) = do
    let primero  = head lista
    putStr(showBicicleta (primero, pUbicacion))
    showBicicletas(tail lista, pUbicacion)


--------------------------------------------------------------------------

--------------------------------------------------------------------------
showUsuario :: Usuario -> [Char]
showUsuario usuario=
    let 
        ced = getCedulaUsuario(usuario)
        nombre =  getNombreUsuario(usuario)
    in
        "\nCedula: " ++ show ced ++ "\tnombre: " ++ show nombre


showUsuarios :: [Usuario] -> IO()
showUsuarios [] = print("")
showUsuarios lista =
    do
        putStr(showUsuario (head lista))
        showUsuarios(tail lista)


--------------------------------------------------------------------------

--------------------------------------------------------------------------

main = do


    putStrLn("Indique la ruta de parqueos: ")
    lParqueos <- getLine

    putStrLn("Indique la ruta de bicicletas: ")
    lBicicletas <- getLine
    

    putStrLn("Indique la ruta de usuarios: ")
    lUsuarios <- getLine

    menuPrincipal (-1, lParqueos, lBicicletas, lUsuarios)