# NDVI FROM LANDSAT 5 - FROM GOOGLE EARTH ENGINE

library(terra)        # rasters
library(raster)       # rasters
library(ggplot2)      
library(viridis)      
library(RColorBrewer) 
library(dplyr)        
library(purrr)        

# Clean
rm(list = ls())

# 1. Parameters --------------

# Path
ruta_imagenes <- "data/GEE_Landsat5_01-12/"
setwd(ruta_imagenes)

# Years
years <- 1986:2010
#months <- c(5, 6, 7)  # Mayo, Junio, Julio
months <- c(1:4,8:12)

# Damn coordinates
lon_centro <- -8.2727195
lat_centro <- 30.639084

# Folder for outputs
dir.create("data/NDVI_Results_01-12", showWarnings = FALSE)
dir.create("data/NDVI_Results/Maps", showWarnings = FALSE)
dir.create("data/NDVI_Results/Statistics", showWarnings = FALSE)

# 2. Functions ------

# NDVI
calculate_ndvi <- function(raster_path) {
  cat("Processing:", basename(raster_path), "\n")
  
  # import raster
  img <- rast(raster_path)
  
  # Name bands
  names(img) <- c("Red", "NIR")
  
  # NDVI: (NIR - Red) / (NIR + Red)
  ndvi <- (img$NIR - img$Red) / (img$NIR + img$Red)
  names(ndvi) <- "NDVI"
  
  # Na if not in range
  ndvi[ndvi < -1 | ndvi > 1] <- NA
  
  return(ndvi)
}


extract_ndvi_stats <- function(ndvi_raster, year, month) {
  values <- values(ndvi_raster, na.rm = TRUE)
  
  if (length(values) == 0) {
    return(data.frame(
      Year = year,
      Month = month,
      Mean = NA,
      Median = NA,
      SD = NA,
      Min = NA,
      Max = NA,
      Q25 = NA,
      Q75 = NA,
      Valid_Pixels = 0
    ))
  }
  
  data.frame(
    Year = year,
    Month = month,
    Mean = mean(values, na.rm = TRUE),
    Median = median(values, na.rm = TRUE),
    SD = sd(values, na.rm = TRUE),
    Min = min(values, na.rm = TRUE),
    Max = max(values, na.rm = TRUE),
    Q25 = quantile(values, 0.25, na.rm = TRUE),
    Q75 = quantile(values, 0.75, na.rm = TRUE),
    Valid_Pixels = length(values)
  )
}

#  3. Look for name files -----

# Name based on structure Aoulouz_L5_RedNIR_YYYY_MM
patron_archivos <- "Aoulouz_L5_RedNIR_.*\\.tif$"
archivos_imagenes <- list.files(pattern = patron_archivos, full.names = TRUE)


# 4. Process --------------------

ndvi_results <- list()
stats_results <- list()

# Loop by file
for (i in seq_along(archivos_imagenes)) {
  archivo <- archivos_imagenes[i]
  nombre_archivo <- basename(archivo)
  
  cat("Processing file ", i, "of ", length(archivos_imagenes), "\n")
  
  # Extract year and month
  partes <- strsplit(nombre_archivo, "_")[[1]]
  
  if (length(partes) >= 5) {
    year <- as.numeric(partes[4])
    month <- as.numeric(gsub("\\.tif$", "", partes[5]))
  } else {
    year <- as.numeric(substr(nombre_archivo, nchar(nombre_archivo)-10, nchar(nombre_archivo)-7))
    month <- as.numeric(substr(nombre_archivo, nchar(nombre_archivo)-5, nchar(nombre_archivo)-4))
  }
  
  cat("Year:", year, "- Month:", month, "\n")
  
  # NDVI
  tryCatch({
    ndvi_raster <- calculate_ndvi(archivo)
    
    if (!is.null(ndvi_raster)) {
      # write raster # change according to route
      output_name <- paste0("../NDVI_Results_01-12/NDVI_",
                            year, "_",
                            sprintf("%02d", month), ".tif")
      writeRaster(ndvi_raster, output_name, overwrite = TRUE)
      
      
      cat("NDVI saved\n")
      
      
    } else {
      cat("Error\n")
    }
    
  }, error = function(e) {
    cat("Error:", e$message, "\n")
  })
}



