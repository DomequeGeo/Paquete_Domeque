# nitro_functions.R
# Para realizar este paquete, me he basado en el script creado para el trabajo de la asignatura de Geoestadística

# Funciones para preparar e interpolar datos de nitrógeno en suelo

#' 1. Preparar datos de nitrógeno
#'
#' Limpia los datos y convierte unidades de cg/kg a g/kg
#'
#' @param df data.frame con columnas x, y y nitrogeno_suelo
#'
#' @return data.frame limpio, con valores de nitrogeno_suelo convertidos, sin duplicados y sin NA
#' @importFrom dplyr distinct
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = c(1,2,3),
#'   y = c(4,5,6),
#'   nitrogeno_suelo = c(200,300,400)
#' )
#' nitro_prepare(df)

nitro_prepare <- function(df){
  df$nitrogeno_suelo <- df$nitrogeno_suelo / 100
  df <- df |> dplyr::distinct() |> na.omit()
  return(df)
}

#' 2. Interpolación IDW de nitrógeno
#'
#' Realiza interpolación IDW usando coordenadas x, y y valores de nitrogeno_suelo
#'
#' @param df data.frame con columnas x, y y nitrogeno_suelo
#' @param raster_base raster de referencia (SpatRaster) para la interpolación
#'
#' @return raster interpolado con valores de nitrógeno estimados
#' @importFrom gstat gstat
#' @importFrom terra interpolate mask rast values
#' @export
#'
#' @examples
#' library(terra)
#' df <- data.frame(
#'   x = c(1,2,3),
#'   y = c(4,5,6),
#'   nitrogeno_suelo = c(200,300,400)
#' )
#' df <- nitro_prepare(df)
#' r <- rast(nrows=5, ncols=5, xmin=0, xmax=5, ymin=0, ymax=5)
#' values(r) <- 1
#' resultado <- nitro_idw(df, r)
#' plot(resultado)

nitro_idw <- function(df, raster_base){

  gs <- gstat::gstat(
    formula = nitrogeno_suelo ~ 1,
    locations = ~ x + y,
    data = df,
    nmax = 15,
    set = list(idp = 2)
  )

  idw <- terra::interpolate(raster_base, gs)
  idw <- terra::mask(idw, raster_base)

  return(idw)
}
