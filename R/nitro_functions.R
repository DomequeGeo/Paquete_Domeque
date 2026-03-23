# nitro_functions.R
# Funciones para preparar e interpolar datos de nitrógeno en suelo

#' Preparar datos de nitrógeno
#'
#' Limpia los datos y convierte unidades de cg/kg a g/kg
#'
#' @param df data.frame con columnas x, y y nitrogeno_suelo
#'
#' @return data.frame limpio, con valores de nitrogeno_suelo convertidos, duplicados eliminados y sin NA
#' @importFrom dplyr distinct
#' @export
nitro_prepare <- function(df){
  df$nitrogeno_suelo <- df$nitrogeno_suelo / 100
  df <- df |> dplyr::distinct() |> na.omit()
  return(df)
}

#' Interpolación IDW de nitrógeno
#'
#' Realiza interpolación IDW usando coordenadas x, y y valores de nitrogeno_suelo
#'
#' @param df data.frame con columnas x, y y nitrogeno_suelo
#' @param raster_base raster de referencia (SpatRaster) para la interpolación
#'
#' @return raster interpolado (SpatRaster) con valores de nitrógeno estimados
#' @importFrom gstat gstat
#' @importFrom terra interpolate mask rast
#' @export
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

# Ejemplo de uso (opcional, se puede quitar en producción)
if(FALSE){
  library(dplyr)
  library(gstat)
  library(terra)

  # Preparar datos
  df <- data.frame(
    x = c(1,2,3),
    y = c(4,5,6),
    nitrogeno_suelo = c(200, 300, 400)
  )
  df_clean <- nitro_prepare(df)
  print(df_clean)

  # Crear raster de ejemplo
  r <- rast(nrows=3, ncols=3, xmin=0, xmax=3, ymin=0, ymax=3)

  # Interpolación IDW
  idw_raster <- nitro_idw(df_clean, r)
  plot(idw_raster, main="Interpolación IDW de nitrógeno")
}

