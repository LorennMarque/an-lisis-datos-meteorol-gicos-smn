#

# rm(list = ls()) 
# options(scipen=999)

# # Obtener directorio del script actual y modificar el working directory
# library("rstudioapi")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(progress)

directorio_datos = "data/smn-data"
# Nombres de columnas
nombres_columnas = c("FECHA", "HORA", "TEMP", "HUM", "PNM", "DD", "FF", "NOMBRE")
# Anchos de las columnas
anchos_columnas <- c(8, 6, 6, 6, 7, 6, 8, 53)
# Clases (tipos) de las columnas
# Forzar FECHA a tipo character, para evitar que lo convierta a numérico y el primer "0" se borre
clases_columnas <- c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "character")

# Listar todos los archivos que siguen el patrón "datohorario*.txt"
archivos <- list.files(path = directorio_datos, pattern = "datohorario.*\\.txt", full.names = TRUE)

# Crear un objeto de progreso
pb <- progress_bar$new(
  format = "Procesando :current/:total [:bar] :percent en :elapsedfull (:tick_rate/s - ETA: :eta)",
  total = length(archivos),
  clear = FALSE,
  width = 80
)

# Función para leer cada archivo
leer_archivo <- function(archivo) {
  pb$tick()  # Avanza la barra de progreso
  
  datos <- read.fwf(archivo, 
                    col.names = nombres_columnas,
                    widths = anchos_columnas, 
                    colClasses = clases_columnas,
                    fileEncoding = "ISO-8859-1", 
                    skip = 2,    # saltear dos primeras líneas
                    strip.white = TRUE,  # borrar espacios en campos char
                    dec = ".")
  
  # Borrar filas con FECHA vacía o NA
  datos <- datos |> filter(FECHA != "" & !is.na(FECHA))
  
  # Convertir la columna FECHA a formato Date con el patrón ddmmYYYY
  datos$FECHA <- as.Date(datos$FECHA, format = "%d%m%Y")
  
  # Convertir FF a numérico
  datos$FF <- as.numeric(datos$FF)
  
  return(datos)
}

# Leer y combinar todos los archivos en un solo dataframe
df <- do.call(bind_rows, lapply(archivos, leer_archivo))

# En vez de guardar un archivo csv, usar formato RDS, más abajo
# Guardar el dataframe df en un archivo CSV
# write.csv(df, "C://clean_msn//datos_smn.csv", row.names = FALSE)

# Guardar el dataframe en formato RDS con compresión
saveRDS(df, file = "datos_smn.rds")

# Leer el archivo RDS
df_rds <- readRDS("datos_smn.rds")
#head(df_rds)


# Verificar rangos, valores extremos, NA's, etc
# summary(df)
#  [HOA]  [ºC]   [%]  [hPa]  [gr] [km/hr]

# Wikipedia: La presión atmosférica no es estable y oscila entre los 885 hPa entre los ciclones 
# más profundos (bajas presiones) y los 1077 hPa sobre los anticiclones siberianos más potentes (altas presiones).
# The highest sea-level pressure on Earth occurs in Siberia, where the Siberian High often
# attains a sea-level pressure above 1,050 hPa (15.2 psi; 31 inHg), 
# with record highs close to 1,085 hPa (15.74 psi; 32.0 inHg). 
# The lowest measurable sea-level pressure is found at the centres of tropical 
# cyclones and tornadoes, with a record low of 870 hPa (12.6 psi; 26 inHg).