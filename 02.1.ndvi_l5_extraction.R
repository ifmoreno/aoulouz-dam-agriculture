# EXTRACT NDVI FOR GRID 
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(parallel)     # Para makeCluster(), stopCluster()
library(doParallel)   # Para registerDoParallel()  
library(foreach)      # Para %dopar%

rm(list = ls())
gc()

# 1. Data -------

# map 

squares <- sf::st_read("data/plots/plots_names.gpkg", layer = "combinado") %>% 
  select(-layer,-path) %>% 
  group_by(key_) %>%
  summarise(
    geom = st_union(geom),
    across(-geom, ~ first(.x)),  # conserva la primera observación de cada variable
    .groups = "drop"
  )
  
# Path where NDVI rasters
ruta_ndvi <- "data/NDVI_Results/"
#setwd(ruta_ndvi)

# Buscar archivos NDVI
archivos_ndvi <- list.files(path = ruta_ndvi,
                            pattern = "^NDVI_\\d{4}_\\d{2}\\.tif$", 
                            full.names = TRUE)


# Convert to SpatVector for terra
squares_vect <- vect(squares)

# 2. Function----

extraer_ndvi <- function(archivo_raster, grid_vector, campo_id = "id") {
  # Cargar raster
  ndvi <- rast(archivo_raster)
  # Extraer ambas estadísticas 
  stats <- extract(ndvi, grid_vector, fun = function(x) {
    if (all(is.na(x))) {
      return(c(NA, 0))  # mediana, conteo
    } else {
      valid_vals <- x[!is.na(x)]
      return(c(median(valid_vals), length(valid_vals)))
    }
  }, bind = T)
  
  # Acceder por nombre de columna 
  return(setNames(data.frame(
    stats[, campo_id], 
    stats[, "NDVI"],   # Primera estadística (mediana)
    stats[, "NDVI.1"]  # Segunda estadística (conteo)
  ),c("id", 
      "ndvi_mediana",
      "pixeles_validos")))
}


# 3. Pararel procesing -----

cat("Cores:", detectCores(), "\n")

# Use 6 cores 
n_cores <- min(6, detectCores() - 2)

# Configurar cluster
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Cargar terra en cada worker
clusterEvalQ(cl, {
  library(terra)
})

# Exportar objetos necesarios a workers
clusterExport(cl, c("squares", "extraer_ndvi"))

cat("Procesando", length(archivos_ndvi), "rasters en paralelo...\n")
tiempo_total_inicio <- Sys.time()

# Procesamiento paralelo
resultados <- foreach(i = seq_along(archivos_ndvi), 
                      .packages = c("terra"),
                      .combine = 'list',
                      .multicombine = TRUE) %dopar% {
                        
                        # CREAR squares_vect DENTRO del worker
                        squares_vect_local <- vect(squares)  # Crear localmente
                        
                        archivo <- archivos_ndvi[i]
                        nombre <- basename(archivo)
                        partes <- strsplit(gsub("\\.tif$", "", nombre), "_")[[1]]
                        
                        # Usar el objeto local
                        stats <- extraer_ndvi(archivo, squares_vect_local, campo_id = "key_")  # Usar objeto local
                        stats$year <- as.numeric(partes[2])
                        stats$month <- as.numeric(partes[3])
                        
                        stats <- stats[, c("id", "year", "month", "ndvi_mediana", "pixeles_validos")]
                        return(stats)
                      }

# Cerrar cluster
stopCluster(cl)

tiempo_total_fin <- Sys.time()
tiempo_total <- round(as.numeric(difftime(tiempo_total_fin, tiempo_total_inicio, units = "mins")), 1)

cat("Time ", tiempo_total, "minutes\n")
cat("Per raster:", round(tiempo_total/length(archivos_ndvi)*60, 1), "seconds\n")


# Combine -------------------------------

library(purrr)

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


dfs <- extract_dfs(resultados)
datos_finales <- do.call(rbind, dfs)

rm(i, archivos_ndvi, n_cores, ruta_ndvi, tiempo_total,
   tiempo_total_fin, tiempo_total_inicio, cl, el,
   nested, out, resultados, dfs, squares_vect, extract_dfs,
   extraer_ndvi)

save.image("data/workspace_plot_lv.RData")
write.csv(datos_finales, "data/plot_lv.csv", row.names = FALSE)



