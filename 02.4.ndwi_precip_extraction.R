# EXTRACT NDWI AND PRECIPITATION FOR GRID 
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(parallel)     
library(doParallel)   
library(foreach)      

rm(list = ls())
gc()


# 1. DATA -------


# Grid/Plots
squares <- sf::st_read("data/plots/plots_names.gpkg", layer = "combinado") %>% 
  select(-layer, -path) %>% 
  group_by(key_) %>%
  summarise(
    geom = st_union(geom),
    across(-geom, ~ first(.x)),
    .groups = "drop"
  )

# Paths donde están los rasters
ruta_ndwi <- "D:/Goettingen/Semestre IV/Tesis/data/rasters/GEE_NDWI/"
ruta_precip <- "D:/Goettingen/Semestre IV/Tesis/data/rasters/GEE_CHIRPS"

# Buscar archivos NDWI
archivos_ndwi <- list.files(
  path = ruta_ndwi,
  pattern = "^Aoulouz_NDWI_\\d{4}_\\d{2}\\.tif$", 
  full.names = TRUE
)

# Buscar archivos Precipitación
archivos_precip <- list.files(
  path = ruta_precip,
  pattern = "^Aoulouz_Precip_\\d{4}_\\d{2}\\.tif$", 
  full.names = TRUE
)

# 2. FUNCTIONS ------------------------------


extraer_ndwi <- function(archivo_raster, grid_vector, campo_id = "id") {
  # Cargar raster
  ndwi <- rast(archivo_raster)
  
  # Extraer estadísticas (mediana y conteo de píxeles válidos)
  stats <- extract(ndwi, grid_vector, fun = function(x) {
    if (all(is.na(x))) {
      return(c(NA, 0))  # mediana, conteo
    } else {
      valid_vals <- x[!is.na(x)]
      return(c(median(valid_vals), length(valid_vals)))
    }
  }, bind = TRUE)
  
  # Retornar dataframe con resultados
  return(setNames(data.frame(
    stats[, campo_id], 
    stats[, "NDWI"],      # Mediana
    stats[, "NDWI.1"]     # Conteo píxeles
  ), c("id", "ndwi_mediana", "pixeles_validos")))
}


extraer_precip <- function(archivo_raster, grid_vector, campo_id = "id") {
  # Cargar raster
  precip <- rast(archivo_raster)
  
  # Extraer estadísticas (media y conteo)
  # Nota: Para precipitación usamos media porque es una suma mensual
  stats <- extract(precip, grid_vector, fun = function(x) {
    if (all(is.na(x))) {
      return(c(NA, 0))  # media, conteo
    } else {
      valid_vals <- x[!is.na(x)]
      return(c(mean(valid_vals), length(valid_vals)))
    }
  }, bind = TRUE)
  
  # Retornar dataframe con resultados
  return(setNames(data.frame(
    stats[, campo_id], 
    stats[, "precipitation"],      # Media (o primer valor si solo hay 1)
    stats[, "precipitation.1"]     # Conteo píxeles
  ), c("id", "precip_mm", "pixeles_validos")))
}

# 3. PARALLEL PROCESSING - NDWI --------------------

n_cores <- min(6, detectCores() - 2)

# Configurar cluster
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Cargar terra en cada worker
clusterEvalQ(cl, {
  library(terra)
})

# Exportar objetos necesarios
clusterExport(cl, c("squares", "extraer_ndwi"))

cat("Procesando", length(archivos_ndwi), "rasters NDWI en paralelo...\n")
tiempo_inicio <- Sys.time()

# Procesamiento paralelo NDWI
resultados_ndwi <- foreach(i = seq_along(archivos_ndwi), 
                           .packages = c("terra"),
                           .combine = 'list',
                           .multicombine = TRUE) %dopar% {
                             
                             # Crear squares_vect localmente en cada worker
                             squares_vect_local <- vect(squares)
                             
                             archivo <- archivos_ndwi[i]
                             nombre <- basename(archivo)
                             
                             # Extraer año y mes del nombre: Aoulouz_NDWI_YYYY_MM.tif
                             partes <- strsplit(gsub("\\.tif$", "", nombre), "_")[[1]]
                             year <- as.numeric(partes[3])
                             month <- as.numeric(partes[4])
                             
                             # Extraer estadísticas
                             stats <- extraer_ndwi(archivo, squares_vect_local, campo_id = "key_")
                             stats$year <- year
                             stats$month <- month
                             
                             # Reordenar columnas
                             stats <- stats[, c("id", "year", "month", "ndwi_mediana", "pixeles_validos")]
                             return(stats)
                           }

stopCluster(cl)

tiempo_fin <- Sys.time()
tiempo_total <- round(as.numeric(difftime(tiempo_fin, tiempo_inicio, units = "mins")), 1)

cat("Time ", tiempo_total, "minutes\n")
cat("Per raster:", round(tiempo_total/length(archivos_ndvi)*60, 1), "seconds\n")


# 4. PARALLEL PROCESSING - PRECIP --------

# Reconfigurar cluster
cl <- makeCluster(n_cores)
registerDoParallel(cl)

clusterEvalQ(cl, {
  library(terra)
})

clusterExport(cl, c("squares", "extraer_precip"))

tiempo_inicio <- Sys.time()

# Procesamiento paralelo Precipitación
resultados_precip <- foreach(i = seq_along(archivos_precip), 
                             .packages = c("terra"),
                             .combine = 'list',
                             .multicombine = TRUE) %dopar% {
                               
                               # Crear squares_vect localmente
                               squares_vect_local <- vect(squares)
                               
                               archivo <- archivos_precip[i]
                               nombre <- basename(archivo)
                               
                               # Extraer año y mes del nombre: Aoulouz_Precip_YYYY_MM.tif
                               partes <- strsplit(gsub("\\.tif$", "", nombre), "_")[[1]]
                               year <- as.numeric(partes[3])
                               month <- as.numeric(partes[4])
                               
                               # Extraer estadísticas
                               stats <- extraer_precip(archivo, squares_vect_local, campo_id = "key_")
                               stats$year <- year
                               stats$month <- month
                               
                               # Reordenar columnas
                               stats <- stats[, c("id", "year", "month", "precip_mm", "pixeles_validos")]
                               return(stats)
                             }

stopCluster(cl)

tiempo_fin <- Sys.time()
tiempo_total <- round(as.numeric(difftime(tiempo_fin, tiempo_inicio, units = "mins")), 1)

cat("Time ", tiempo_total, "minutes\n")
cat("Per raster:", round(tiempo_total/length(archivos_ndvi)*60, 1), "seconds\n")

# =============================================================================
# 5. COMBINAR Y LIMPIAR RESULTADOS ----
# =============================================================================

cat("COMBINANDO RESULTADOS\n")

# Función para extraer dataframes de listas anidadas
extract_dfs <- function(x) {
  out <- list()
  if (is.data.frame(x)) return(list(x))
  if (!is.list(x)) return(list())
  for (el in x) {
    if (is.data.frame(el)) {
      out[[length(out) + 1]] <- el
    } else if (is.list(el)) {
      nested <- extract_dfs(el)
      if (length(nested) > 0) out <- c(out, nested)
    }
  }
  out
}

# Extraer y combinar NDWI
dfs_ndwi <- extract_dfs(resultados_ndwi)
datos_ndwi <-do.call(rbind, dfs_ndwi)

# Extraer y combinar Precipitación
dfs_precip <- extract_dfs(resultados_precip)
datos_precip <- do.call(rbind, dfs_precip)


# Renombrar columnas para evitar conflictos en el merge
colnames(datos_ndwi)[colnames(datos_ndwi) == "pixeles_validos"] <- "pixeles_validos_ndwi"
colnames(datos_precip)[colnames(datos_precip) == "pixeles_validos"] <- "pixeles_validos_precip"

# Combinar NDWI y Precipitación en un solo dataframe
datos_finales <- datos_ndwi %>%
  full_join(datos_precip, by = c("id", "year", "month"))


# =============================================================================
# 6. GUARDAR RESULTADOS ----
# =============================================================================

# Limpiar workspace


# Guardar workspace y CSV
save.image("data/workspace_plot_ndwi_precip.RData")
write.csv(datos_finales, "data/plot_ndwi_precip.csv", row.names = FALSE)

rm(i, archivos_ndwi, archivos_precip, n_cores, ruta_ndwi, ruta_precip,
   tiempo_total, tiempo_fin, tiempo_inicio, cl, el, nested, 
   resultados_ndwi, resultados_precip, dfs_ndwi, dfs_precip,
   datos_ndwi, datos_precip, extract_dfs, extraer_ndwi, extraer_precip)

