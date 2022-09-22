import Data.Char


---información de la empresa
--type NombreEmpresa =  String
--type SitioWeb = String
--type Contacto = Integer
--type TarifaKmPedal = Float
--type TarifaKmElectrico = Float
data Empresa = Empresa {
    nombreEmpresa::String,
    sitioWeb::String,
    contacto::Integer,
    tarifaKmPedal::Float,
    tarifaKmElectrico::Float
}deriving(Eq);


getNombreEmpresa(Empresa nombre _ _ _ _) = nombre;
getWebEmpresa(Empresa _ web _ _ _) = web;
getContactoEmpresa(Empresa _ _ contacto _ _) = contacto;
getTarKmPedal(Empresa _ _ _ pedal _) = pedal;
getTarKmEle(Empresa _ _ _ _ electrico) = electrico;
----------------------------------------------------------------------------------------


---parqueos
--type NombreParqueo = String
--type DireccionParqueo = String
--type ProvinciaParqueo = String
--type UbicacionX = Float
--type UbicacionY = Float
data Parqueo = Parqueo {
    nombreParqueo ::String,
    direccionParqueo :: String,
    provinciaParqueo ::String,
    ubicacionX:: Float,
    ubicacionY :: Float
} deriving(Eq);

crearParqueo(elemento) = Parqueo (elemento!!0) (elemento!!1) (elemento!!2) (read(elemento!!3) :: Float) (read(elemento!!4) :: Float)
getNombreParqueo (Parqueo nombre _ _ _ _) = nombre;
getDireccionParqueo (Parqueo _ direccion _ _ _) = direccion;
getProvinciaParqueo (Parqueo _ _ provincia _ _) = provincia;
getUbicacionX (Parqueo _ _ _ x _) = x;
getUbicacionY (Parqueo _ _ _ _ y) = y;

------------------------------------------------------------------------------------------
--bicicletas
--type IdentificadorBicicleta = String
--type TipoBicicleta = String
--type ParqueoBicicleta = String
data Bicicleta = Bicicleta {
    identificadorBicicleta::String,
    tipoBicicleta::String,
    parqueoBicicleta::String
}deriving(Eq);

crearBicicleta(elemento) = Bicicleta (elemento!!0) (elemento!!1) (elemento!!2)
getIdBicicleta (Bicicleta id _ _) = id;
getTipoBicicleta (Bicicleta _ tipo _) = tipo;
getParqueoBicicleta ( Bicicleta _ _ par) = par;
----------------------------------------------------------

--usuarios
type Cedula =  Integer
type Nombre = String
data Usuario = Usuario Cedula Nombre;

crearUsuario(elemento) = Usuario (read(elemento!!0) :: Integer) (elemento!!1)
getCedulaUsuario(Usuario ced _) = ced;
getNombreUsuario (Usuario _ nombre) = nombre;
-------------------------------------------------------------------------------------
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
                1-> showParqueos lParqueos
                2-> showBicicletas lBicicletas
                3-> do
                    usuarios <- cargarUsuarios lUsuarios
                    putStrLn("Se han cargado los siguiente usuarios")
                    showUsuarios usuarios
                4-> putStrLn("4. Estadisticas")

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


menuGeneral (opcion, lParqueos, lBicicletas, lUsuarios) =
    if opcion == 4 then
        print ("volviendo")
    else
        do
            case opcion of
                -1-> putStrLn ("")
                1-> do
                    putStrLn("1. Consultar bicicletas")
                    consultarBicicletas lParqueos
                2-> do
                    putStrLn("2. Alquilar")
                    alquilar( lParqueos, lBicicletas, lUsuarios)
                3-> putStrLn("3. Facturar")

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

-- guardar en un txt
alquilar(lParqueos, lBicicletas, lUsuarios) = do
    putStrLn ("\n\nalquiler")
    usuarios <- cargarUsuarios lUsuarios
    showUsuarios usuarios
    usuario <- seleccionarUsuario usuarios
    if usuario == "#" then 
        print ("Se ha cancelado la operación")
    else do
        showParqueos lParqueos
        putStrLn "Seleccione el parqueo de salida"
        parqueoSalida <- seleccionarParqueoS lParqueos
        parqueoLlegada <- seleccionarParqueoL (lParqueos, parqueoSalida)
        let bicicletasParqueo =getBicicletasParqueo(lBicicletas, parqueoSalida)
        showBicicletas(bicicletasParqueo)
        bicicleta <- seleccionarBicicleta(bicicletasParqueo)
        putStrLn( "Cedula: " ++ usuario ++"\
                    \ \nSalida: " ++ parqueoSalida ++ "\
                    \ \nLlegada: "++ parqueoLlegada++ "\
                    \ \nBicicleta: "++ bicicleta)
        
-------------------------------------------------------------------------------------------

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
seleccionarParqueoS lParqueos = do
    putStrLn "Ingrese el nombre del parqueo de salida o (#) para cancelar el alquiler: "
    parqueoSalida <- getLine
    if parqueoSalida == "#" then 
        return parqueoSalida
    else do
        if existeParqueo(lParqueos, parqueoSalida) then
            return parqueoSalida
        else do
            putStrLn "El parqueo no existe"
            seleccionarParqueoS lParqueos


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

----------------------------------------
cargarBicicletas :: FilePath-> IO [Bicicleta] 
cargarBicicletas archivo = do
        contenido <- readFile archivo
        let lBicicletas = separaBicicletas (toLines contenido)
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

showBicicleta :: Bicicleta -> [Char]
showBicicleta bicicleta=
    let 
        id = getIdBicicleta(bicicleta)
        tipo =  getTipoBicicleta(bicicleta)
        parqueo = getParqueoBicicleta(bicicleta)
    in
        "\nIdentificador: " ++ show id ++ "\ttipo: " ++ show tipo ++ "\tParqueo: " ++ show parqueo


showBicicletas :: [Bicicleta] -> IO()
showBicicletas [] = print("")
showBicicletas lista =
    do
        putStr(showBicicleta (head lista))
        showBicicletas(tail lista)

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
    parqueos <-cargarParqueos lParqueos

    putStrLn("Indique la ruta de bicicletas: ")
    lBicicletas <- getLine
    bicicletas <-cargarBicicletas lBicicletas

    putStrLn("Indique la ruta de usuarios: ")
    lUsuarios <- getLine

    menuPrincipal (-1, parqueos, bicicletas, lUsuarios)