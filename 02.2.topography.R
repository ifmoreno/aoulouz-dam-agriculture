# ELEVATION, SLOPE, ASPECT

library(terra)
library(sf)
library(dplyr)

rm(list = ls())
gc()

# 1. Data --------------------------------------------------------------------

load("data/data_prep_plot_lv.RData")
umbral <- 10

squares <- datos_geom

centroides <- st_centroid(squares) 
centroides_vect <- vect(centroides)  
 
# Buscar archivos topográficos individuales
elevation <- rast("data/Topographic_Data/Aoulouz_Elevation_SRTM30m.tif")
slope <- rast("data/Topographic_Data/Aoulouz_Slope_SRTM30m.tif")
aspect <- rast("data/Topographic_Data/Aoulouz_Aspect_SRTM30m.tif")

elevation <- project(elevation, crs(squares))
slope <- project(slope, crs(squares))
aspect <- project(aspect, crs(squares))



# Extraer valores topográficos
centroides$elevation <- terra::extract(elevation, centroides_vect)[,2]
centroides$slope <- terra::extract(slope, centroides_vect)[,2] 
centroides$aspect <- terra::extract(aspect, centroides_vect)[,2]


# Extraer valor Dam
dam_aoulouz <- st_read("data/composition.gpkg", layer = "dam_Aoulouz") 
dam_aoulouz_utm <- st_transform(dam_aoulouz, st_crs(squares))

elev_dam <- terra::extract(elevation, dam_aoulouz_utm)[,2]

centroides$elev_relative <- centroides$elevation - elev_dam

# dataframe
topo <- centroides %>% 
  select(key_,elevation, slope,aspect, elev_relative) %>% 
  mutate(
    slope_category = cut(slope,
                         breaks = c(-Inf, 2, 5, 10, 15, Inf),
                         labels = c("flat", "gentle", "moderate", "steep", "very_steep")),
    favorable_aspect = aspect >= 90 & aspect <= 225,
    optimal_topo = (slope > 2 & slope < 8) & (elev_relative > -50 & elev_relative < 100))

area_plot <- datos_finales %>%
  st_drop_geometry() %>% 
  select(id, area, area_cat) %>% 
  distinct()

topo <- topo %>% 
  left_join(area_plot, by = c("key_"="id"))

rm(list = c("aspect","centroides","centroides_vect",
            "elevation","slope", "elev_dam", "dam_aoulouz",
            "dam_aoulouz_utm", "squares", "area_plot"))

gc()

save.image("data/workspace_plot_lv.RData")



