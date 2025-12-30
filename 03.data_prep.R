library(tidyverse)
library(sf)
# Consolidate -----
rm(list = ls())
gc()
load("data/workspace_plot_lv.RData")

gc()

# dam
dam_x  <- st_coordinates(dam_aoulouz_utm)[,1]
dam_y <- st_coordinates(dam_aoulouz_utm)[,2]

# precipitation & NDWI
precip_annual <- read.csv("data/plot_ndwi_precip.csv") %>% 
  select(1:4,6)

# area squares
squares$area <- as.numeric(st_area(squares))

# calculate centroids for each cell
# calculate different variables related to the position of each centroid
centroides <- st_centroid(squares) %>% 
  mutate(
    coord_x = st_coordinates(.)[, "X"],
    coord_y = st_coordinates(.)[, "Y"],
    distancia_represa = as.numeric(st_distance(., st_centroid(dam_aoulouz_utm))),
    distance_km = distancia_represa / 1000,
    distance_zone = case_when(
      distance_km <= 2 ~ "Very close (0-2km)",
      distance_km <= 5 ~ "Close (2-5km)",
      distance_km <= 10 ~ "Medium (5-10km)",
      distance_km <= 15 ~ "Far (10-15km)",
      TRUE ~ "Very far (>15km)"
    ),
    angle = atan2(coord_y - dam_y, coord_x - dam_x) * 180/pi,
    direction = case_when(
      angle >= -45 & angle < 45 ~ "East",
      angle >= 45 & angle < 135 ~ "North", 
      angle >= 135 | angle < -135 ~ "West",
      angle >= -135 & angle < -45 ~ "South"
    ), 
    area_cat = case_when(
      area < 5000 ~ "Less than 0.5ha",
      area >= 5000 & area < 10000 ~ "Between 0.5 & 1 ha",
      area >= 10000 & area < 15000 ~ "Between 1 & 1.5 ha",
      area >= 15000 & area < 20000 ~ "Between 1.5 & 2 ha",
      area >= 20000 & area < 25000 ~ "Between 2 & 2.5 ha",
      area >= 25000  ~ "More than 2.5 ha" 
    ),
    ,
    area_cat = factor(
      area_cat,
      levels = c(
        "Less than 0.5ha",
        "Between 0.5 & 1 ha",
        "Between 1 & 1.5 ha",
        "Between 1.5 & 2 ha",
        "Between 2 & 2.5 ha",
        "More than 2.5 ha"
      ),
      ordered = TRUE
    )
  ) 
  


# This dataset has the ndvi calculated per month, per year, per plot
datos_finales <- left_join(datos_finales, 
                           centroides %>% st_drop_geometry(), by = c("id"="key_")) %>% 
  mutate(
    detailed_period = case_when(
      year < 1991 ~ "Pre-dam (1984-90)",
      year >= 1991 & year <= 2002 ~ "Aoulouz only (1991-2002)", 
      year > 2002 ~ "Full system (2003-10)"
    ),
    # ADD VARIABLES FOR ECONOMETRIC ANALYSIS
    post_aoulouz = ifelse(year >= 1990, 1, 0),
    post_full_system = ifelse(year > 2002, 1, 0),
    # INTERACTIONS WITH DISTANCE
    dist_post_aoulouz = distance_km * post_aoulouz,
    dist_post_system = distance_km * post_full_system
  ) %>% 
  left_join(precip_annual, by = c("id","year","month"))  
  


datos_geom <- squares %>% select(key_:id_bloc ) 

rm(list = grep("^datos_", ls(), invert = TRUE, value = TRUE))

umbral <- 10

# dry season ---------------------------------------------------------------------

dry_months <- c(5:8)

df_dry <- datos_finales %>%
  filter(year < 1998) %>% 
  # months novem - may
  mutate(grow_month = ifelse(month %in% dry_months,1,0)) %>%
  filter(grow_month ==1) %>% 
  group_by(id, year) %>%
  summarise(
    max_ndvi = max(ndvi_mediana, na.rm = TRUE),
    mean_ndvi = mean(ndvi_mediana, na.rm = TRUE),
    max_ndwi = max(ndwi_mediana, na.rm = TRUE),
    mean_ndwi = mean(ndwi_mediana, na.rm = TRUE),
    sum_rain = sum(precip_mm, na.rm = TRUE),
    mean_rain = mean(precip_mm, na.rm = TRUE),
    area = mean(area, na.rm = TRUE),
    area_cat = first(area_cat, na.rm = TRUE),
    distance_km = first(distance_km),
    AUEA = first(AUEA),
    coord_x = first(coord_x),
    coord_y = first(coord_y),
    .groups = "drop"
  ) %>%
  mutate(
    treat = ifelse(distance_km <= umbral, 1, 0),
    event_time = year - 1991,
    post = ifelse(year >= 1991, 1, 0),
    treat_post = treat * post,
    precip_x_near = sum_rain * treat,
    precip_x_post = sum_rain * post,
    precip_x_near_post = sum_rain * treat * post,
    block = str_sub(id,1,5),
    distance_bin = case_when(
      distance_km <= 3 ~ "0-3km",
      distance_km <= 6 ~ "3-6km", 
      distance_km <= 9 ~ "6-9km",
      distance_km <= 12 ~ "9-12km",
      distance_km <= 15 ~ "12-15km",
      TRUE ~ "15+km"
    ), 
    distance_bin = factor(distance_bin, levels = c("0-3km", "3-6km", "6-9km", "9-12km", "12-15km", "15+km"))
  ) 


# season 1 ---------------------------------------------------------------------

grow_months <- c(11,12,1,2,3,4,5)


df_season_1 <- datos_finales %>%
  filter(year < 1998) %>% 
  # months novem - may
  mutate(grow_month = ifelse(month %in% grow_months,1,0)) %>%
  mutate(grow_year_season = ifelse(month<6, year-1,year),
  ) %>% 
  filter(grow_month ==1) %>% 
  group_by(id, grow_year_season) %>%
  summarise(
    max_ndvi = max(ndvi_mediana, na.rm = TRUE),
    mean_ndvi = mean(ndvi_mediana, na.rm = TRUE),
    max_ndwi = max(ndwi_mediana, na.rm = TRUE),
    mean_ndwi = mean(ndwi_mediana, na.rm = TRUE),
    sum_rain = sum(precip_mm, na.rm = TRUE),
    mean_rain = mean(precip_mm, na.rm = TRUE),
    distance_km = first(distance_km),
    AUEA = first(AUEA),
    coord_x = first(coord_x),
    coord_y = first(coord_y),
    .groups = "drop"
  ) %>%
  mutate(
    treat = ifelse(distance_km <= umbral, 1, 0),
    event_time = grow_year_season - 1991,
    post = ifelse(grow_year_season >= 1991, 1, 0),
    treat_post = treat * post,
    precip_x_near = sum_rain * treat,
    precip_x_post = sum_rain * post,
    precip_x_near_post = sum_rain * treat * post,
    block = str_sub(id,1,5),
    distance_bin = case_when(
      distance_km <= 3 ~ "0-3km",
      distance_km <= 6 ~ "3-6km", 
      distance_km <= 9 ~ "6-9km",
      distance_km <= 12 ~ "9-12km",
      distance_km <= 15 ~ "12-15km",
      TRUE ~ "15+km"
    ), 
    distance_bin = factor(distance_bin, levels = c("0-3km", "3-6km", "6-9km", "9-12km", "12-15km", "15+km"))
  )

# season 2 ---------------------------------------------------------------------

grow_months <- c(6:10)


df_season_2 <- datos_finales %>%
  filter(year < 1998) %>% 
  # months novem - may
  mutate(grow_month = ifelse(month %in% grow_months,1,0)) %>%
  mutate(grow_year_season = year) %>% 
  filter(grow_month ==1) %>% 
  group_by(id, grow_year_season) %>%
  summarise(
    max_ndvi = max(ndvi_mediana, na.rm = TRUE),
    mean_ndvi = mean(ndvi_mediana, na.rm = TRUE),
    max_ndwi = max(ndwi_mediana, na.rm = TRUE),
    mean_ndwi = mean(ndwi_mediana, na.rm = TRUE),
    sum_rain = sum(precip_mm, na.rm = TRUE),
    mean_rain = mean(precip_mm, na.rm = TRUE),
    distance_km = first(distance_km),
    AUEA = first(AUEA),
    coord_x = first(coord_x),
    coord_y = first(coord_y),
    .groups = "drop"
  ) %>%
  mutate(
    treat = ifelse(distance_km <= umbral, 1, 0),
    event_time = grow_year_season - 1991,
    post = ifelse(grow_year_season >= 1991, 1, 0),
    treat_post = treat * post,
    precip_x_near = sum_rain * treat,
    precip_x_post = sum_rain * post,
    precip_x_near_post = sum_rain * treat * post,
    block = str_sub(id,1,5),
    distance_bin = case_when(
      distance_km <= 3 ~ "0-3km",
      distance_km <= 6 ~ "3-6km", 
      distance_km <= 9 ~ "6-9km",
      distance_km <= 12 ~ "9-12km",
      distance_km <= 15 ~ "12-15km",
      TRUE ~ "15+km"
    ), 
    distance_bin = factor(distance_bin, levels = c("0-3km", "3-6km", "6-9km", "9-12km", "12-15km", "15+km"))
  )



save.image("data/data_prep_plot_lv.RData")
