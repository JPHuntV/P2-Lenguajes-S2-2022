module Estructuras where

data Empresa = Empresa {
    nombreEmpresa::String,
    sitioWebEmpresa::String,
    contactoEmpresa::Integer,
    tarifaKmPedalEmpresa::Float,
    tarifaKmElectricoEmpresa::Float
}deriving(Eq);

-- Constructor de Empresa
crearEmpresa(elemento) = Empresa (elemento!!0) (elemento!!1) (read (elemento!!2) :: Integer) (read(elemento!!3) :: Float) (read(elemento!!4) :: Float)
getNombreEmpresa(Empresa nombre _ _ _ _) = nombre;
getWebEmpresa(Empresa _ web _ _ _) = web;
getContactoEmpresa(Empresa _ _ contacto _ _) = contacto;
getTarKmPedal(Empresa _ _ _ pedal _) = pedal;
getTarKmEle(Empresa _ _ _ _ electrico) = electrico;
----------------------------------------------------------------------------------------

data Parqueo = Parqueo {
    nombreParqueo ::String,
    direccionParqueo :: String,
    provinciaParqueo ::String,
    ubicacionXParqueo:: Float,
    ubicacionYParqueo :: Float
} deriving(Eq);

-- Constructor Parqueo
crearParqueo(elemento) = Parqueo (elemento!!0) (elemento!!1) (elemento!!2) (read(elemento!!3) :: Float) (read(elemento!!4) :: Float)
getNombreParqueo (Parqueo nombre _ _ _ _) = nombre;
getDireccionParqueo (Parqueo _ direccion _ _ _) = direccion;
getProvinciaParqueo (Parqueo _ _ provincia _ _) = provincia;
getUbicacionX (Parqueo _ _ _ x _) = x;
getUbicacionY (Parqueo _ _ _ _ y) = y;

------------------------------------------------------------------------------------------
data Bicicleta = Bicicleta {
    identificadorBicicleta::String,
    tipoBicicleta::String,
    parqueoBicicleta::String
}deriving(Eq);

--Constructor Bicicleta
crearBicicleta(elemento) = Bicicleta (elemento!!0) (elemento!!1) (elemento!!2)
getIdBicicleta (Bicicleta id _ _) = id;
getTipoBicicleta (Bicicleta _ tipo _) = tipo;
getParqueoBicicleta ( Bicicleta _ _ par) = par;
----------------------------------------------------------

--usuarios

data Usuario = Usuario{
    cedulaUsuario::Integer,
    nombreUsuario::String
}deriving(Eq);

--Constructor Usuario
crearUsuario(elemento) = Usuario (read(elemento!!0) :: Integer) (elemento!!1)
getCedulaUsuario(Usuario ced _) = ced;
getNombreUsuario (Usuario _ nombre) = nombre;
-------------------------------------------------------------------------------------

--alquiler
data Alquiler = Alquiler {
    codigoAlquiler::Integer,
    cedulaAlquiler::Integer,
    pSalidaAlquiler::String,
    pLlegadaAlquiler::String,
    idBicicletaAlquiler::String,
    estadoAlquiler::String
}deriving(Eq);

-- Constructor Alquiler
crearAlquiler(elemento) = Alquiler (read(elemento!!0) :: Integer) (read(elemento!!1) :: Integer) (elemento!!2) (elemento!!3) (elemento!!4) (elemento!!5)
getCodigoAlquiler(Alquiler cod _ _ _ _ _) = cod;
getCedulaAlquiler(Alquiler _ ced _ _ _ _) = ced;
getSalidaAlquiler(Alquiler _ _ salida _ _ _) = salida;
getLlegadaAlquiler(Alquiler _ _ _ llegada _ _) = llegada;
getIdBicicletaAlquiler(Alquiler _ _ _ _ idBici _) = idBici;
getEstadoAlquiler(Alquiler _ _ _ _ _ estado) = estado;

--------------------------------------------------

--factura
data Factura = Factura{
    codigoFactura :: Integer,
    usuarioFactura :: Integer,
    pSalidaFact :: String,
    pLlegadaFact :: String,
    biciFactura :: String,
    tipoBiciFactura :: String,
    cantKM :: Float,
    tarifaKM:: Float,
    total ::Float
}deriving(Eq);

--Constructor Factura
crearFactura(elemento) = Factura (read(elemento!!0) :: Integer) (read(elemento!!1) :: Integer) (elemento!!2) (elemento!!3) (elemento!!4) (elemento!!5) (read(elemento!!6) :: Float) (read(elemento!!7) :: Float) (read(elemento!!8) :: Float)
getCodigoFactura (Factura cod _ _ _ _ _ _ _ _) = cod;
getUsuarioFactura (Factura _ usuario _ _ _ _ _ _ _) = usuario;
getPSalidaFact (Factura _ _ pSalida _ _ _ _ _ _) = pSalida;
getPLlegadaFact (Factura _ _ _ pLlegada _ _ _ _ _) = pLlegada ;
getBiciFactura (Factura _ _ _ _ bici _ _ _ _) = bici ;
getTipoBiciFactura (Factura _ _ _ _ _ tipoBici _ _ _) = tipoBici;
getCantKM (Factura _ _ _ _ _ _ cantKm _ _) = cantKm;
getTarifaKMFactura (Factura _ _ _ _ _ _ _ tarifaKm _) =tarifaKm ;
getTotalFactura (Factura _ _ _ _ _ _ _ _ total) = total;