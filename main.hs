import Data.Char
import Data.Typeable
import Data.List --funcion filter
import System.Exit --salir del sistema
import System.Directory -- verificar si archivo existe

---------------------------
--https://stackoverflow.com/questions/22166912/how-to-close-a-file-in-haskell
--Necesitaba poder escribir al archivo
--evaluate(force file) solucionó el problema
import Control.DeepSeq-----
import Control.Exception---
---------------------------
import Estructuras



infoComercial = crearEmpresa(["nombreEmpresa", "webEmpresa","12345", "2234.2", "55.3"])


------------------------------------------------------------------------------------------------
-- ######################################## Menus ########################################### --
------------------------------------------------------------------------------------------------


{----------------------------------------
Nombre: menuPrincipal
E: opcion -> opcion seleccionada, (lParqueos, lBicicletas, lUsuarios) -> rutas de los archivos con las tablas correspondientes
S: Ejecución de la función seleccionada
R: Se debe seleccionar una opción disponible
O: Despliega un menu de opciones que redirigen a distintas funciones
Nota: Se utilizo el formato de menú visto en clase
----------------------------------------}

menuPrincipal (opcion, lParqueos, lBicicletas, lUsuarios) =
    if opcion == 3 then do
        salir
    else do
        case opcion of
            -1-> putStr("")
            1-> menuOperativo (-1,lParqueos, lBicicletas, lUsuarios)
            2-> menuGeneral (-1,lParqueos, lBicicletas, lUsuarios)

        putStrLn("\n\n\nMenú principal \
                \ \n1. Opciones operativas\
                \ \n2. Opciones generales\
                \ \n3. Salir\
                \ \n\nSeleccione una opción: ")
        tempOpcion <- getLine
        let opcion = (read tempOpcion :: Integer)
        menuPrincipal(opcion, lParqueos, lBicicletas, lUsuarios)


{----------------------------------------
Nombre: menuOperativo
E: opcion -> opcion seleccionada, (lParqueos, lBicicletas, lUsuarios) -> rutas de los archivos con las tablas correspondientes
S: Ejecución de la función seleccionada
R: Se debe seleccionar una opción disponible
O: Despliega un menu de opciones que redirigen a distintas funciones
Nota: Se utilizo el formato de menú visto en clase
----------------------------------------}

menuOperativo (opcion, lParqueos, lBicicletas, lUsuarios) =
    if opcion == 5 then
        putStrLn ("\nVolviendo al menú principal ....\n")
    else do
        parqueos <-cargarParqueos lParqueos --
        usuarios <- cargarUsuarios lUsuarios --Listas de estructuras de las tablas correspondientes
        bicicletas <- cargarBicicletas lBicicletas --
        case opcion of
            -1-> putStrLn ("")
            1-> do
                putStrLn ("\n\n\n#################### Parqueos ####################")
                showParqueos parqueos
                putStrLn"\n--------------------------------------------------\n\n\n"
            2-> do
                putStrLn ("\n\n\n#################### Parqueos ####################")
                showParqueos parqueos
                putStrLn"\n--------------------------------------------------\n\n\n"
                mostrarBicicletas bicicletas
            3-> do
                putStrLn ("\n\n\n#################### Usuarios ####################")
                showUsuarios usuarios
                putStrLn"\n--------------------------------------------------\n\n\n"
            4-> do
                menuEstadisticas(-1, parqueos, bicicletas, usuarios)
            6-> salir

        putStrLn("\n\n\nMenú operativo\
                \ \n1. Mostrar parqueos\
                \ \n2. Mostrar bicicletas\
                \ \n3. Cargar usuarios\
                \ \n4. Estadísticas\ 
                \ \n5. Volver\
                \ \n6. Salir\
                \ \nSeleccione una opción: ")

        tempOpcion <- getLine
        let opcion = (read tempOpcion :: Integer)
        menuOperativo(opcion, lParqueos, lBicicletas, lUsuarios) --Se envia la ruta, no la lista, esto para que se actualizen los datos 


{----------------------------------------
Nombre: menuGeneral
E: opcion -> opcion seleccionada, (lParqueos, lBicicletas, lUsuarios) -> rutas de los archivos con las tablas correspondientes
S: Ejecución de la función seleccionada
R: Se debe seleccionar una opción disponible
O: Despliega un menu de opciones que redirigen a distintas funciones
Nota: Se utilizo el formato de menú visto en clase
----------------------------------------}

menuGeneral (opcion, lParqueos, lBicicletas, lUsuarios) =
    if opcion == 4 then
        putStrLn ("\nVolviendo al menú principal ....\n")
    else do
        parqueos <-cargarParqueos lParqueos --
        bicicletas <-cargarBicicletas lBicicletas --Listas de estructuras de las tablas correspondientes
        case opcion of
            -1-> putStrLn ("")
            1-> do
                putStrLn ("\n\n\n#################### Consulta de bicicletas ####################") 
                consultarBicicletas (parqueos, bicicletas)
            2-> do
                putStrLn ("\n\n\n#################### Alquiler de bicicletas ####################")  
                alquilar( parqueos, bicicletas, lUsuarios)
            3-> do
                putStrLn ("\n\n\n#################### Facturación ####################") 
                facturar (bicicletas,parqueos)
            5->salir
        putStrLn("\n\n\nMenú operativo\ 
                \ \n1. Consultar bicicletas\ 
                \ \n2. Alquilar\
                \ \n3. Facturar\
                \ \n4. Volver\
                \ \n5. Salir\
                \ \nSeleccione una opción: ")
        tempOpcion <- getLine
        let opcion = (read tempOpcion :: Integer)
        menuGeneral(opcion, lParqueos, lBicicletas, lUsuarios) --Se envia la ruta, no la lista, esto para que se actualizen los datos 


{----------------------------------------
Nombre: menuEstadisticas
E: opcion -> opcion seleccionada, (lParqueos, lBicicletas, lUsuarios) -> rutas de los archivos con las tablas correspondientes
S: Ejecución de la función seleccionada
R: Se debe seleccionar una opción disponible
O: Despliega un menu de opciones que redirigen a distintas funciones
Nota: Se utilizo el formato de menú visto en clase
----------------------------------------}

menuEstadisticas (opcion, lParqueos, lBicicletas, lUsuarios) =
    if opcion == 5 then
        putStrLn ("\nVolviendo al menu general ....\n")
    else do
        lFacturas <- cargarFacturas "facturas.txt"
        case opcion of
            -1-> putStrLn ("")
            1-> do
                putStrLn ("\n\n\n#################### Top 5 usuarios ####################\n\n\n")
                getTop5Usuarios (lFacturas, lUsuarios)
                putStrLn"\n--------------------------------------------------\n\n\n"
            2-> do
                putStrLn ("\n\n\n#################### Top 5 parqueos ####################\n\n\n")
                getTop5Parqueos (lFacturas,lParqueos)
                putStrLn"\n--------------------------------------------------\n\n\n"
            3-> do
                putStrLn ("\n\n\n#################### Top 3 bicicletas ####################\n\n\n")
                getTop3Bicicletas(lFacturas,lBicicletas)
                putStrLn"\n--------------------------------------------------\n\n\n"
            4-> do
                putStrLn ("\n\n\n#################### Resumen ####################\n\n")
                resumen(lFacturas)
                putStrLn"\n\n--------------------------------------------------\n\n\n"
            6-> salir
        putStrLn("\nMenú estadísticas\
                \ \n1. Top 5 usuarios con más viajes\
                \ \n2. Top 5 parqueos con más viajes\
                \ \n3. Top 3 bicicletas con más kilometros recorridos\
                \ \n4. Resumen\
                \ \n5. Volver\
                \ \n6. Salir\
                \ \nSeleccione una opción: ")
        tempOpcion <- getLine
        let opcion = (read tempOpcion :: Integer)
        menuEstadisticas (opcion, lParqueos, lBicicletas, lUsuarios)--Se envia la ruta, no la lista, esto para que se actualizen los datos 
------------------------------------------------------------------------------------------------
--
--
------------------------------------------------------------------------------------------------
-- ######################################## Parqueos ######################################## --
------------------------------------------------------------------------------------------------


{----------------------------------------
Nombre: cargarParqueos
E: Ruta del archivo que contiene la tabla parqueos
S: Una lista de estructuras Parqueo
R: El archivo debe tener el formato correcto
O: Recibe una ruta de un archivo y crea una lista de Parqueos con la información de dicho archivo
Nota: Se utilizó el metodo de carga visto en clase
----------------------------------------}

cargarParqueos :: FilePath-> IO[Parqueo] 
cargarParqueos archivo = do
        contenido <- readFile archivo
        let lParqueos = separaParqueos (toLines contenido)
        return lParqueos


{----------------------------------------
Nombre: separaParqueos
E: Lista de las lineas de un archivo 
S: Una lista de Parqueos
R: La linea debe tener el formato correcto 
O: Recibe una lista de lineas de texto, la divide por comas y crea un parqueo
Nota: Se utilizó el metodo de separación visto en clase
----------------------------------------}

separaParqueos :: [[Char]]-> [Parqueo]
separaParqueos (lista) = 
    if lista == [] then
        []
    else
        [crearParqueo(separarPorComas((head lista), ""))] ++ separaParqueos (tail lista)


{----------------------------------------
Nombre: showParqueo
E: parqueo -> Un Parqueo
S: String con la información del parqueo en formato para imprimir
R: Debe recibir un Parqueo
O: Recibe un parqueo y retorna una cadena de texto con su información
Nota: Se utilizo el metodo de impresión de estructuras visto en clase
----------------------------------------}

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


{----------------------------------------
Nombre: showParqueos
E: lista -> una lista de tipo Parqueo
S: Imprime la información de los parqueos en la lista
R: Debe recibir una lista tipo Parqueo
O: Imprimir la información de los parqueos
Nota: Se utilizo el metodo de impresión de estructuras visto en clase
----------------------------------------}

showParqueos :: [Parqueo] -> IO()
showParqueos [] = putStrLn("")
showParqueos lista =
    do
        putStr(showParqueo (head lista))
        showParqueos(tail lista)


{----------------------------------------
Nombre: getParqueoCercano
E:(x1, y1) -> una cordenada, lParqueos->lista de parqueos, cercano-> tomará el valor del más cercano
S: nombre del parqueo más cercano
R: x1 y y1 deben ser valores numericos
O: Recibe una cordenada y una lista de parqueos, la recorre y retorna el parqueo más cercano a la coordenada
----------------------------------------}

getParqueoCercano (x1, y1, lParqueos,cercano) = do
    if lParqueos == [] then do
        let 
            nombre =  getNombreParqueo(cercano)
            direccion = getDireccionParqueo(cercano)
            provincia = getProvinciaParqueo(cercano)
            x = getUbicacionX(cercano)
            y = getUbicacionY(cercano)
        putStrLn ("\n\n\n--------------------------------\
                    \ \n\nEl parqueo más cercano es: \
                    \ \n\nParqueo: " ++ show nombre ++ "\
                    \ \nDireccion: " ++ show direccion ++ "\
                    \ \nProvincia: " ++ show provincia ++ "\
                    \ \nX: " ++ show x ++ "\
                    \ \nY: " ++ show y ++ "\
                    \ \n\n-----------------------------------")
        return nombre
    else
        do
        let par = head lParqueos
        if calcularDistanciaParqueo(x1,y1,par)<= calcularDistanciaParqueo(x1,y1,cercano) then
            getParqueoCercano(x1,y1,(tail lParqueos),par)
        else
            getParqueoCercano(x1,y1,(tail lParqueos),cercano)


{----------------------------------------
Nombre:  calcularDistanciaParqueo
E: (x1, y1)-> punto 1 , parqueo -> un Parqueo
S: distancia del punto 1 al parqueo
R: x1 y y1 deben ser numericos y parqueo tipo Parqueo
O: Obtiene la ubicación del parqueo y calcula la distancia desde el punto al parqueo
----------------------------------------}

calcularDistanciaParqueo(x1,y1, parqueo) =do 
    let x2 = getUbicacionX(parqueo)
    let y2 = getUbicacionY(parqueo)
    calcularDistancia(x1,x2,y1,y2)


{----------------------------------------
Nombre: seleccionarParqueS
E: lParqueos ->lista de parqueos, lBicicletas -> lista bicicletas
S: Nombre del parqueo seleccionado
R: El nombre debe ser exacto, esto quiere decir que es sensible a mayusculas
O: Solicita el nombre de un parqueo , lo retorna si este existe y tiene bicicletas
----------------------------------------}

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


{----------------------------------------
Nombre: seleccionarParqueL
E: lParqueos ->lista de parqueos, lBicicletas -> lista bicicletas
S: Nombre del parqueo seleccionado
R: El nombre debe ser exacto, esto quiere decir que es sensible a mayusculas
O: Solicita el nombre de un parqueo , lo retorna si este existe
----------------------------------------}

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


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

existeParqueo ([], nombre) = False
existeParqueo (lParqueos, nombre)= do
    let primero = (head lParqueos)
    let nombreTemp = getNombreParqueo(primero)
    if nombre == nombreTemp then
        True
    else
        existeParqueo((tail lParqueos), nombre)


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

getParqueoXNombre(nombre, lParqueos) = do
    let primero = head lParqueos
    let nombreTemp = getNombreParqueo(primero)
    if nombre == nombreTemp then
        return primero
    else
        getParqueoXNombre(nombre, tail lParqueos)


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

getTop5Parqueos (lFacturas, lParqueos)= do
    let lViajesParqueo = getViajesXParqueo(lFacturas, lParqueos)
    let ordenada = sortBy(\x1 x2 -> compare (read (last x2) :: Integer) (read (last x1) ::Integer)) lViajesParqueo --https://stackoverflow.com/questions/19587518/ordering-a-list-of-lists-in-haskell
    let top5 = take 5 ordenada
    imprimirListaTop (top5,"Parqueo","Cantidad de viajes salida/llegada")


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

getViajesXParqueo (lFacturas, lParqueos) = do
    if lParqueos == [] then
        []
    else do
        let elemento = head lParqueos
        let nombreParqueo = getNombreParqueo(elemento)
        let cantViajes = getViajesXParqueoAux(nombreParqueo, lFacturas)
        [[nombreParqueo]++[show cantViajes]] ++ getViajesXParqueo(lFacturas, tail lParqueos)


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

getViajesXParqueoAux (nombreParqueo, lFacturas) = do
    if lFacturas == [] then
        0
    else do
        let elemento = head lFacturas
        let salidaTemp = getPSalidaFact(elemento)
        let llegadaTemp = getPLlegadaFact(elemento)
        if nombreParqueo == salidaTemp || nombreParqueo == llegadaTemp then
            1 + getViajesXParqueoAux(nombreParqueo, tail lFacturas)
        else
            getViajesXParqueoAux(nombreParqueo, tail lFacturas)

------------------------------------------------------------------------------------------------
--
--
------------------------------------------------------------------------------------------------
-- ####################################### Alquileres ####################################### --
------------------------------------------------------------------------------------------------


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

cargarAlquileres :: FilePath-> IO [Alquiler] 
cargarAlquileres archivo = do
        contenido <- readFile archivo
        let lAlquileres = separarAlquileres (toLines contenido)
        evaluate(force contenido)
        return lAlquileres


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

separarAlquileres :: [[Char]]  -> [Alquiler]        
separarAlquileres (lista) = 
    if lista == [] then
        []
    else
        [crearAlquiler(separarPorComas((head lista), ""))] ++ separarAlquileres (tail lista)


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

alquilar(lParqueos, lBicicletas, lUsuarios) = do
    usuarios <- cargarUsuarios lUsuarios
    putStrLn ("\n\n\n#################### Usuarios ####################")
    showUsuarios usuarios
    putStrLn"\n--------------------------------------------------\n"
    usuario <- seleccionarUsuario usuarios
    if usuario == "#" then do
        print ("Se ha cancelado la operación")
    else do
        putStrLn ("\n\n\n#################### Parqueos ####################")
        showParqueos lParqueos
        putStrLn"\n--------------------------------------------------\n"
        parqueoSalida <- seleccionarParqueoS (lParqueos,lBicicletas)
        if parqueoSalida == "#" then do
            print ("Se ha cancelado la operación")
        else do
            putStrLn("\n")
            parqueoLlegada <- seleccionarParqueoL (lParqueos, parqueoSalida)
            if parqueoLlegada == "#" then do
                print ("Se ha cancelado la operación")
            else do
                let bicicletasParqueo =getBicicletasParqueo(lBicicletas, parqueoSalida)
                putStrLn ("\n\n\n#################### Bicicletas ####################")
                showBicicletas(lBicicletas,parqueoSalida,0)
                putStrLn"\n--------------------------------------------------\n"
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
                    putStrLn( "\n\n¡Se ha generado el alguiler!\
                                \ \n\n----------------------------------------\
                                \ \nCodigo: " ++ show cantAlquileres ++"\ 
                                \ \nCedula: " ++ usuario ++"\
                                \ \nSalida: " ++ parqueoSalida ++ "\
                                \ \nLlegada: "++ parqueoLlegada++ "\
                                \ \nBicicleta: "++ bicicleta ++" \
                                \ \n----------------------------------------")


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

getAlquiler(idAlquiler, lAlquileres) = do
    let primero = head lAlquileres
    let idTemp = getCodigoAlquiler(primero)
    if idAlquiler == idTemp then
         return primero
    else do
        getAlquiler(idAlquiler, tail lAlquileres)


{----------------------------------------
Nombre: seleccionarAlquiler
E: lAlquileres -> lista de alquileres
S: codigo del alquiler seleccionado
R: El nombre debe ser exacto, esto quiere decir que es sensible a mayusculas
O: Solicita el codigo de un alquiler , lo retorna si este existe
----------------------------------------}

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


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

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


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

facturarAlquiler(lAlquileres,alquiler, estado) = do
    writeFile "alquileres.txt" ""
    facturarAlquilerAux(lAlquileres, alquiler, estado)


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

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


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

showAlquilerActivo :: [Alquiler] -> IO()
showAlquilerActivo [] = putStrLn("")
showAlquilerActivo lista = do
    let alquiler = head lista
    let estadoAlquiler = getEstadoAlquiler(alquiler)
    if estadoAlquiler == "activo" then do
        let codigoAlquiler = getCodigoAlquiler(alquiler)
        let cedulaAlquiler = getCedulaAlquiler(alquiler)
        let pSalidaAlquiler = getSalidaAlquiler(alquiler)
        let pLlegadaAlquiler = getLlegadaAlquiler(alquiler)
        let idBicicletaAlquiler = getIdBicicletaAlquiler(alquiler)
        putStrLn( "Codigo: " ++ show codigoAlquiler ++"\ 
                \ usuario: " ++ show cedulaAlquiler ++"\
                \ Salida: " ++ pSalidaAlquiler ++ "\
                \ Llegada: "++ pLlegadaAlquiler++ "\
                \ Bicicleta: "++ idBicicletaAlquiler++"\
                \ Estado: " ++ estadoAlquiler)
        showAlquilerActivo(tail lista)
    else
        showAlquilerActivo(tail lista)
------------------------------------------------------------------------------------------------
--
--
------------------------------------------------------------------------------------------------
-- ####################################### Facturas ######################################### --
------------------------------------------------------------------------------------------------


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

cargarFacturas :: FilePath-> IO [Factura] 
cargarFacturas archivo = do
        contenido <- readFile archivo
        let lFacturas = separarFacturas (toLines contenido)
        evaluate(force contenido)
        return lFacturas


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

separarFacturas :: [[Char]]  -> [Factura]        
separarFacturas (lista) = 
    if lista == [] then
        []
    else
        [crearFactura(separarPorComas((head lista), ""))] ++ separarFacturas (tail lista)


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

facturar(bicicletas,lParqueos) = do
    listaAlquileres <-cargarAlquileres "alquileres.txt"
    putStrLn ("\n\n\n#################### Alquileres Activos ####################\n")
    showAlquilerActivo(listaAlquileres)
    putStrLn"\n--------------------------------------------------\n"
    alquilerTemp <- seleccionarAlquiler listaAlquileres
    if alquilerTemp == "#" then 
        print " Se ha cancelado la operación"
    else do
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
        putStrLn("\n¡El alquiler " ++ alquilerTemp ++ " ha sido facturado!")
        putStrLn ("\nSe ha generado la siguiente factura")
        printFactura(factura)


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

printFactura(factura) = do
    putStrLn ("\n\n#################### Factura ####################")
    putStrLn ("Codigo: " ++show (getCodigoFactura(factura)) ++ "\
    \ \nNombre empresa: " ++ getNombreEmpresa(infoComercial)++"\
    \ \nSitio web: " ++ getWebEmpresa(infoComercial) ++ "\
    \ \nContacto: " ++ show (getContactoEmpresa(infoComercial)) ++ "\
    \ \nUsuario: " ++ show (getUsuarioFactura(factura)) ++ "\
    \ \nParqueo Salida: " ++ getPSalidaFact(factura) ++ "\
    \ \tParqueo Llegada: " ++ getPLlegadaFact(factura) ++ "\
    \ \nidBicicleta: "++ getBiciFactura(factura)++ "\
    \ \ttipoBicicleta: "++ (if getTipoBiciFactura(factura) =="Ae" then "asistencia electrica" else "pedales") ++ "\
    \ \nDistancia recorrida: " ++ show (getCantKM(factura)) ++"\
    \ \nTarifa x Kilometro: " ++ show (getTarifaKMFactura(factura))++ "\
    \ \nTotal colones: " ++ show ( getTotalFactura(factura)))
    putStrLn"\n--------------------------------------------------\n"

{----------------------------------------
Nombre: resumen
E: lFacturas -> Lista de facturas
S: Llamada a su función auxiliar
R: ninguna
O: Solicita el resumen de estadisticas a su función auxiliar
----------------------------------------}

resumen(lFacturas) = do
    resumenAux(lFacturas, 0,0,0)


{----------------------------------------
Nombre: resumenAux
E: lFacturas -> lista de facturas, (totalViajes, totalKm, totalFact) ->contadores para dichos valores
S: Impresión del resumen estadistico (total de viajes, total de km recorridos y total facturado en colones)
R: (totalViajes, totalKm, totalFact) deben iniciar en 0
O: Calcula e imprime el resumen de facturas
----------------------------------------}

resumenAux(lFacturas, totalViajes, totalKm, totalFact) = do
    if lFacturas == [] then do
        putStrLn("Cantidad viajes terminados: " ++ show totalViajes ++ "\
                \ \nTotal de kilometros: " ++ show totalKm ++ "\
                \ \nTotal facturado: " ++ show totalFact)
    else do
        let factura = head lFacturas
        let cantKmFact = getCantKM(factura)
        let total = getTotalFactura(factura)
        resumenAux(tail lFacturas, totalViajes+1, totalKm+cantKmFact , totalFact+total)


------------------------------------------------------------------------------------------------
--
--
------------------------------------------------------------------------------------------------
-- ####################################### Bicicletas ####################################### --
------------------------------------------------------------------------------------------------


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

cargarBicicletas :: FilePath-> IO [Bicicleta] 
cargarBicicletas archivo = do
        contenido <- readFile archivo
        let lBicicletas = separaBicicletas (toLines contenido)
        evaluate(force contenido)
        return lBicicletas


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

separaBicicletas :: [[Char]]  -> [Bicicleta]        
separaBicicletas (lista) = 
    if lista == [] then
        []
    else
        [crearBicicleta(separarPorComas((head lista), ""))] ++ separaBicicletas (tail lista)


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

showBicicleta :: (Bicicleta,String,Integer) -> [Char]
showBicicleta (bicicleta,pUbicacion,hayBicicletas)=
    let 
        id = getIdBicicleta(bicicleta)
        tipo =  getTipoBicicleta(bicicleta)
        parqueo = getParqueoBicicleta(bicicleta)
    in
        if pUbicacion =="#" ||pUbicacion == parqueo then do
            "\nIdentificador: " ++ show id ++ "\ttipo: " ++ show tipo ++ "\tParqueo: " ++ show parqueo
        else ""


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

showBicicletas :: ([Bicicleta],String,Integer) -> IO()
showBicicletas ([],pUbicacion,hayBici) = do
    if hayBici == 0 then
        putStrLn("\n\n\n---No existen bicicletas en esta ubicación---\n\n\n")
    else
        putStrLn "\n"


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

showBicicletas (lista ,pUbicacion, hayBici) = do
    let primero  = head lista
    let id = getIdBicicleta(primero)
    let tipo =  getTipoBicicleta(primero)
    let parqueo = getParqueoBicicleta(primero)
    if pUbicacion =="#" || pUbicacion == parqueo then do 
        putStr("\nIdentificador: " ++ show id ++ "\t\ttipo: " ++ show tipo ++ "\tParqueo: " ++ show parqueo)
        showBicicletas(tail lista, pUbicacion,1)
    else do 
        putStr ""
        showBicicletas(tail lista, pUbicacion,hayBici)
              

{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

consultarBicicletas (lParqueos,lBicicletas) = do
    putStrLn "Indique x: "
    pX<- getLine
    putStrLn "Indique y: "
    pY<- getLine
    parqueo <- getParqueoCercano (read (pX) ::Float, read (pY) ::Float, lParqueos, (head lParqueos))
    putStrLn ("\n\n\n#################### Bicicletas ####################")    
    showBicicletas(lBicicletas,parqueo,0)
    putStrLn"\n--------------------------------------------------\n"


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

getBicicletasParqueo (lBicicletas, nombreParqueo) = do
    if lBicicletas == [] then 
        []
    else do
        let elemento = head lBicicletas
        if getParqueoBicicleta(elemento) == nombreParqueo then
            [elemento] ++ getBicicletasParqueo(tail lBicicletas, nombreParqueo)
        else
            getBicicletasParqueo(tail lBicicletas, nombreParqueo)


{----------------------------------------
Nombre: seleccionarBicicleta
E: lBicicletas -> lista de bicicletas
S: codigo de la bicicleta seleccionado
R: El nombre debe ser exacto, esto quiere decir que es sensible a mayusculas
O: Solicita el codigo de una bicicleta , lo retorna si este existe
----------------------------------------}

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


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

existeBicicleta ([], idBicicleta) = False
existeBicicleta (lBicicletas, idBicicleta)= do
    let primero = (head lBicicletas)
    let idTemp = getIdBicicleta(primero)
    if idBicicleta == idTemp then
        True
    else
        existeBicicleta((tail lBicicletas), idBicicleta)


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

bicicletaUbicacion(lBicicletas,bicicleta, ubicacion)= do
    writeFile "bicicletas.txt" ""
    bicicletaUbicacionAux (lBicicletas,bicicleta, ubicacion)


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

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


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

getTipoBicicleta2(idBicicleta, bicicletas) = do
    let primero = head bicicletas
    let tipo = getTipoBicicleta(primero)
    let idTemp = getIdBicicleta(primero)
    if idBicicleta == idTemp then
        tipo
    else do
        getTipoBicicleta2(idBicicleta, tail bicicletas)


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

getTop3Bicicletas (lFacturas, lBicicletas)= do
    let lDistanciaBici = getDistanciaBici(lFacturas, lBicicletas)
    let ordenada = sortBy(\x1 x2 -> compare (read (last x2) ::Float) (read(last x1) ::Float)) lDistanciaBici --https://stackoverflow.com/questions/19587518/ordering-a-list-of-lists-in-haskell
    let top3 = take 3 ordenada
    imprimirListaTop (top3,"Bicicleta", "Cantidad de Km recorridos")


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

getDistanciaBici (lFacturas, lBicicletas) = do
    if lBicicletas == [] then
        []
    else do
        let elemento = head lBicicletas
        let idBici = getIdBicicleta(elemento)
        let distanciaRec = getDistanciaBiciAux(idBici, lFacturas)
        [[idBici]++[show distanciaRec]] ++ getDistanciaBici(lFacturas, tail lBicicletas)


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

getDistanciaBiciAux (idBici, lFacturas) = do
    if lFacturas == [] then
        0
    else do
        let elemento = head lFacturas
        let idBiciTemp = getBiciFactura(elemento)
        let distanciaTemp = getCantKM(elemento)
        if idBici == idBiciTemp then
            distanciaTemp + getDistanciaBiciAux(idBici, tail lFacturas)
        else
            getDistanciaBiciAux(idBici, tail lFacturas)


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

mostrarBicicletas lBicicletas= do
    putStrLn "\n-Ingrese el nombre del parqueo que desea consultar \
    \ \n-'#' Para consultar todas las bicicletas\
    \ \n-o 'transito' para consultar las bicicletas en tránsito\
    \ \nOpción: "
    parqueo <- getLine
    putStrLn ("\n\n\n#################### Bicicletas ####################")
    if parqueo == "transito" then do
        showBicicletas (lBicicletas,"en transito",0)
        putStrLn"\n--------------------------------------------------\n\n\n"    
    else do
        showBicicletas (lBicicletas,parqueo,0)
        putStrLn"\n--------------------------------------------------\n\n\n"    
------------------------------------------------------------------------------------------------
--
--
------------------------------------------------------------------------------------------------
-- ####################################### Usuarios ####################################### --
------------------------------------------------------------------------------------------------


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

cargarUsuarios :: FilePath-> IO [Usuario] 
cargarUsuarios archivo = do
        contenido <- readFile archivo
        let lUsuarios = separaUsuarios (toLines contenido)
        return lUsuarios


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

separaUsuarios :: [[Char]]  -> [Usuario]        
separaUsuarios (lista) = 
    if lista == [] then
        []
    else
        [crearUsuario(separarPorComas((head lista), ""))] ++ separaUsuarios (tail lista)


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

showUsuario :: Usuario -> [Char]
showUsuario usuario=
    let 
        ced = getCedulaUsuario(usuario)
        nombre =  getNombreUsuario(usuario)
    in
        "\nCedula: " ++ show ced ++ "\tnombre: " ++ show nombre


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

showUsuarios :: [Usuario] -> IO()
showUsuarios [] = putStrLn("")
showUsuarios lista =
    do
        putStr(showUsuario (head lista))
        showUsuarios(tail lista)


{----------------------------------------
Nombre: seleccionarUsuario
E: lUsuarios -> lista de usuarios
S: Cedula del usuario seleccionado
R: La cedula debe ser exacta
O: Solicita la cedula de un usuario , lo retorna si este existe
----------------------------------------}

seleccionarUsuario lUsuarios = do
    putStrLn "Ingrese la cedula del usuario o (#) para cancelar el alquiler: "
    cedulaUsuario <- getLine
    if cedulaUsuario == "#" then 
        return cedulaUsuario
    else do
        if (all isDigit cedulaUsuario) && existePersona(lUsuarios, read cedulaUsuario::Integer) then
            return cedulaUsuario
        else do
            putStrLn "El usuario no existe"
            seleccionarUsuario lUsuarios


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

existePersona ([], cedula) = False
existePersona (lUsuarios, cedula)= do
    let primero = (head lUsuarios)
    let cedulaTemp = getCedulaUsuario(primero)
    if cedula == cedulaTemp then
        True
    else
        existePersona((tail lUsuarios), cedula) 


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

getTop5Usuarios (lFacturas,lUsuarios)= do
    
    let lViajesUsuario = getViajesXUsuario(lFacturas, lUsuarios)
    let ordenada = sortBy(\x1 x2 -> compare (read (last x2) :: Integer) (read (last x1) ::Integer)) lViajesUsuario --https://stackoverflow.com/questions/19587518/ordering-a-list-of-lists-in-haskell
    let top5 = take 5 ordenada
    imprimirListaTop (top5,"Usuario", "Cantidad de viajes completados")


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

getViajesXUsuario (lFacturas, lUsuarios) = do
    if lUsuarios == [] then
        []
    else do
        let elemento = head lUsuarios
        let usuario = getCedulaUsuario(elemento)
        let cantViajes = getViajesXUsuarioAux(usuario, lFacturas)
        [[show usuario]++[show cantViajes]] ++ getViajesXUsuario(lFacturas, tail lUsuarios)


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

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
------------------------------------------------------------------------------------------------
--
--
------------------------------------------------------------------------------------------------
-- ####################################### Auxiliares ####################################### --
------------------------------------------------------------------------------------------------


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

toLines :: String -> [String]
toLines texto = lines texto



{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

separarPorComas :: ([Char], [Char]) -> [[Char]]
separarPorComas (cadena, temp) =
    if cadena == "" then [temp] else
        if (head cadena) == (head ",") then
            [temp] ++ separarPorComas((tail cadena), "")
            else
                separarPorComas ((tail cadena), temp++[(head cadena)])


{----------------------------------------
Nombre: 
E:
S:
R:
O:
----------------------------------------}

getInput = do
    x <- getLine
    if x /="" then 
        return x
    else do
        getInput



{----------------------------------------
Nombre: getNombreArchivo
E: Ruta de un archivo mediante consola
S: Cadena de texto con la ruta de un archivo  
R: La ruta debe existir
O: Solicita la ruta de un archivo y la retorna si esta existe
    Si no existe, se vuelve a intentar o se cierra el programa
----------------------------------------}

getNombreArchivo = do
    archivo <- getInput
    existe <- doesFileExist archivo
    if existe then
        return archivo
    else do
        putStrLn("Este archivo no existe ¿quiere intentar de nuevo?\n(y/n): ")
        intento <- getInput
        if lowerString intento == "y" then do
            putStrLn("Ruta: ")
            getNombreArchivo
        else
            exitSuccess



{----------------------------------------
Nombre: lowerString
E: cadena de texto
S: cadena en minusculas
R: Debe ser una cadena de texto
O: Traduce una cadena de texto a minuscula
----------------------------------------}

lowerString = map toLower


{----------------------------------------
Nombre: calcularDistancia
E: (x1,y1) -> punto 1, (x2,y2)->punto 2
S: distancia entre punto 1 y punto 2
R: los argumentos deben ser numericos
O: Calcula la distancia entre dos puntos
Nota: Se utiliza la siguiente formula: https://www.neurochispas.com/wp-content/uploads/2021/08/diagrama-para-derivar-la-formula-de-la-distancia-entre-dos-puntos.png
----------------------------------------}

calcularDistancia(x1,x2,y1,y2) = 
    sqrt((x2-x1)**2 + (y2-y1)**2)


{----------------------------------------
Nombre: getDistaciaRecorrida
E: Nombres de los parqueos de salida y llegada y la lista de parqueos
S: Distancia entre dos parqueos
R: los parqueos deben de existir
O: Calcula la distancia entre dos parqueos
----------------------------------------}

getDistaciaRecorrida(pSalida, pLlegada, lParqueos) = do
    parqueoSalida <- getParqueoXNombre(pSalida,lParqueos)
    parqueoLlegada <- getParqueoXNombre(pLlegada, lParqueos)
    let 
        x1 = getUbicacionX(parqueoSalida)
        x2 = getUbicacionX(parqueoLlegada)
        y1 = getUbicacionY(parqueoSalida)
        y2 = getUbicacionY(parqueoLlegada)
        f = calcularDistancia(x1,x2,y1,y2) 
    return f


{----------------------------------------
Nombre: getTarifa
E: tipoTarifa->AE o TR
S: tarifa correspondiente al tipo de bicicleta
R: el valor de tipoTarifa debe ser AE o TR
O: Retorna la tarifa correspondiente al tipo de bicicleta indicado
----------------------------------------}

getTarifa(tipoTarifa) = do
    if tipoTarifa == "AE" then
        getTarKmEle(infoComercial)
    else 
        getTarKmPedal(infoComercial)


{----------------------------------------
Nombre: imprimirListaTop
E: lista-> una lista de listas [a,b], arg1 y arg2 -> cadena de texto que indica que valor imprime
S: Impresión del top
R: ninguna
O: Imprime un top 
----------------------------------------}

imprimirListaTop (lista,arg1,arg2) = do
    if lista == [] then
        return ()
    else do 
        let primero = head lista
        putStrLn (arg1++": " ++ head primero ++"\t"++ arg2++": " ++last primero)
        imprimirListaTop (tail lista,arg1,arg2)



{----------------------------------------
Nombre: salir
E: Ninguna
S: Salida del sistema
R: Ninguna
O: Cierra el programa
----------------------------------------}

salir = do
    putStrLn ("Saliendo del sistema")
    exitSuccess


------------------------------------------------------------------------------------------------
--
--
------------------------------------------------------------------------------------------------
-- ####################################### Main ############################################# --
------------------------------------------------------------------------------------------------


{----------------------------------------
Nombre: main
E: Ninguna
S: Ejecución del programa
R: Ninguna
O: Da inicio a la ejecución del programa y solicita las rutas de los archivos
    que contienen la base de datos
----------------------------------------}

main = do
    putStrLn("\n############### Alquiler de bicicletas ###############\n")
    putStrLn("Indique la ruta de parqueos: ")
    lParqueos <- getNombreArchivo

    putStrLn("Indique la ruta de bicicletas: ")
    lBicicletas <- getNombreArchivo
    

    putStrLn("Indique la ruta de usuarios: ")
    lUsuarios <- getNombreArchivo

    menuPrincipal (-1, lParqueos, lBicicletas, lUsuarios)