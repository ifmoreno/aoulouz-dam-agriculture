library(tidyverse)
library(sf)
library(ggplot2)
library(gridExtra)
library(viridis)
library(corrplot)
library(lubridate)

rm(list = ls())

# 1. Data preparation -----------------------------------------------------

load("data/data_prep_plot_lv.RData")
precip_annual <- read.csv("data/CHIRSPS/chirps_annual_precip_Aoulouz.csv")
scale_factor <- 0.25 / max(datos_finales$annual_total_precip_mm, na.rm = TRUE)


# 2. Visualization --------------------------------------------------------
## 2.0 Number of images Landsat 5 ----------------------------------------------

n_images <- read.csv("data/landsat_5.csv") %>% 
  group_by(Año) %>% 
  summarise(Num_imagenes = sum(Num_Imagenes))
n_images$date <- lubridate::year(n_images$Año)

eventos <- c(1991, 2002)

ggplot(n_images, aes(x = Año, y = Num_imagenes)) +
  geom_col(fill = "steelblue") +
  geom_vline(xintercept = eventos, linetype = "dashed", 
             color = c("red", "darkgreen"), 
             alpha = 0.7) +
  annotate("text", x = 1991, y = 4, label = "Aoulouz Dam", 
           angle = 90, vjust = -0.5, size = 3) +
  annotate("text", x = 2002, y = 4, label = "Full system", 
           angle = 90, vjust = -0.5, size = 3) +
  labs(
    title = "Number of Images per year \nLandsat 5",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/06.data_vis/00.number_images.png")

## 2.01 Precipitation ----------------------------------------------

ggplot(precip_annual) +
  geom_col(data = precip_annual,
           aes(x = year, y = annual_total_precip_mm),
           fill = "lightblue", alpha = 0.3, width = 0.8,
           inherit.aes = FALSE) +
  geom_vline(xintercept = 1991, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_vline(xintercept = 2002, linetype = "dashed", color = "darkgreen", alpha = 0.7) +
  # ANNOTATIONS
  annotate("text", x = 1991, y = 350, label = "Aoulouz Dam", 
           angle = 90, vjust = -0.5, size = 3) +
  annotate("text", x = 2002, y = 350, label = "Full system", 
           angle = 90, vjust = -0.5, size = 3) +
  labs(title = "Annual Precipitation",
       x = "Year",
       y = "Total mm") +
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "darkblue"))

ggsave("output/06.data_vis/00.1.annual_precip.png")

## 2.1 Two periods - line ------------------------------------------------

scale_factor <- 0.25 / max(datos_finales$annual_total_precip_mm, na.rm = TRUE)

# Preparar datos
datos_finales$detailed_period <- ifelse(datos_finales$year < 1991, 
                                        "Pre",
                                        "Post")

gradient_2periods <- datos_finales %>%
  filter(year <= 2002) %>% 
  group_by(detailed_period, distance_km = round(distance_km, 1)) %>%
  summarise(
    ndvi_mean = mean(ndvi_mediana, na.rm = TRUE),
    ndvi_se = sd(ndvi_mediana, na.rm = TRUE) / sqrt(n()),
    n_obs = n(),
    .groups = 'drop'
  ) 

# Gráfico con estilo unificado
ggplot(gradient_2periods,
                     aes(x = distance_km, 
                         y = ndvi_mean, 
                         color = detailed_period,
                         fill = detailed_period)) +
  
  # Ribbon primero (fondo)
  geom_ribbon(aes(ymin = ndvi_mean - ndvi_se, ymax = ndvi_mean + ndvi_se), 
              alpha = 0.2, color = NA) +
  
  # Líneas encima
  geom_line(linewidth = 0.8) +
  
  # Líneas verticales de referencia
  geom_vline(xintercept = c(2, 5, 10), 
             linetype = "dashed", 
             color = "grey30", 
             linewidth = 0.5, 
             alpha = 0.7) +
  
  # Etiquetas para las distancias
  annotate("text", x = 1, y = Inf, label = "Very\nclose", 
           vjust = 1.2, hjust = 0.5, size = 2.5, color = "grey20", lineheight = 0.9) +
  annotate("text", x = 3.5, y = Inf, label = "Close", 
           vjust = 1.2, hjust = 0.5, size = 2.5, color = "grey20", lineheight = 0.9) +
  annotate("text", x = 7.5, y = Inf, label = "Medium", 
           vjust = 1.2, hjust = 0.5, size = 2.5, color = "grey20", lineheight = 0.9) +
  annotate("text", x = 13, y = Inf, label = "Far", 
           vjust = 1.2, hjust = 0.5, size = 2.5, color = "grey20", lineheight = 0.9) +
  
  # Escalas
  scale_x_continuous(
    name = "Distance to dam (km)",
    limits = c(0, 16),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name = "Average NDVI",
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  # Colores profesionales unificados
  scale_color_manual(
    name = "Period",
    values = c("Pre" = "#0072B2", "Post" = "#D55E00")
  ) +
  scale_fill_manual(
    name = "Period",
    values = c("Pre" = "#0072B2", "Post" = "#D55E00")
  ) +
  
  # Tema limpio para publicación (IGUAL al gráfico mensual)
  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    # Ejes
    axis.line = element_line(linewidth = 0.5, color = "black"),
    axis.ticks = element_line(linewidth = 0.5, color = "black"),
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    
    # Leyenda
    legend.position = c(0.02, 0.3),
    #legend.position = c(0.22, 0.42),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "grey50", linewidth = 0.3),
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.1, "lines"),
    legend.margin = margin(4, 6, 4, 6),
    
    # Panel
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    # Márgenes
    plot.margin = margin(10, 15, 10, 10)
  )



ggsave("output/figures/03.p_gradient_periods.pdf", 
       width = 7, height = 4, dpi = 300, bg = "white")


## 2.2 Temporal trend ------------------------------------------------------
zones_time <- datos_finales %>%
  group_by(distance_zone, year) %>%
  summarise(
    ndvi_mean = mean(ndvi_mediana, na.rm = TRUE),
    ndvi_se = sd(ndvi_mediana, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) %>% 
  mutate(distance_zone = factor(distance_zone, 
                                levels = c("Very close (0-2km)",
                                           "Close (2-5km)",      
                                           "Medium (5-10km)",
                                           "Far (10-15km)",  
                                           "Very far (>15km)")))

 

ggplot(zones_time,
       aes(x = year, y = ndvi_mean, color = distance_zone)) +
  # PRECIPITACIÓN - como barras en el fondo
  geom_col(data = precip_annual,
           aes(x = year, y = annual_total_precip_mm * scale_factor),
           fill = "lightblue", alpha = 0.3, width = 0.8,
           inherit.aes = FALSE) +
  # NDVI - líneas originales
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = ndvi_mean - ndvi_se, 
                  ymax = ndvi_mean + ndvi_se, 
                  fill = distance_zone), alpha = 0.2, color = NA) +
  # EVENT LINES
  geom_vline(xintercept = 1991, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_vline(xintercept = 2002, linetype = "dashed", color = "darkgreen", alpha = 0.7) +
  # ANNOTATIONS
  annotate("text", x = 1991, y = 0.20, label = "Aoulouz Dam", 
           angle = 90, vjust = -0.5, size = 3) +
  annotate("text", x = 2002, y = 0.20, label = "Full system", 
           angle = 90, vjust = -0.5, size = 3) +
  # ESCALAS
  scale_color_viridis_d(name = "Zone") +
  scale_fill_viridis_d(name = "Zone") +
  scale_y_continuous(
    name = "Average NDVI",
    sec.axis = sec_axis(~ . / scale_factor, 
                        name = "Annual Precipitation (mm)")
  ) +
  labs(title = "NDVI Temporal Evolution by Distance Zones with Annual Precipitation",
       subtitle = "Confidence band = standard error; Blue bars = precipitation",
       x = "Year") +
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "darkblue")) 

ggsave("output/06.data_vis/02.p_temporal.png")

## 2.2 Temporal trend other style----

# Factor de escala para precipitación (ajusta según tus datos)
scale_factor <- 0.001  # Ajusta este valor según necesites

# Data frame con eventos (estilo consistente)
eventos <- data.frame(
  year = c(1991, 2002),
  label = c("Aoulouz\nDam", "Mokhtar Soussi\nDam")
)

ggplot(zones_time,
       aes(x = year, y = ndvi_mean, color = distance_zone)) +
  
  # EVENTOS - estilo uniforme del plot de referencia
  geom_vline(data = eventos, aes(xintercept = year),
             linetype = "dashed", color = "grey30", linewidth = 0.5, alpha = 0.7) +
  
  # PRECIPITACIÓN - área de fondo (como en el plot de referencia)
  geom_col(data = precip_annual,
           aes(x = year, y = annual_total_precip_mm * scale_factor),
           fill = "grey40", alpha = 0.2, width = 0.8,
           inherit.aes = FALSE) +
  
  # NDVI - Ribbons primero (fondo)
  geom_ribbon(aes(ymin = ndvi_mean - ndvi_se, 
                  ymax = ndvi_mean + ndvi_se, 
                  fill = distance_zone), 
              alpha = 0.2, color = NA) +
  
  # NDVI - Líneas encima
  geom_line(linewidth = 0.8) +  # linewidth en lugar de size
  
  # ETIQUETAS DE EVENTOS - estilo del plot de referencia
  geom_text(data = eventos, 
            aes(x = year, y = Inf, label = label),
            vjust = 1.2, hjust = 0.5, size = 2.5, 
            color = "grey20", lineheight = 0.9,
            inherit.aes = FALSE) +
  
  # ESCALAS
  scale_y_continuous(
    name = "Average NDVI",
    sec.axis = sec_axis(~ . / scale_factor, 
                        name = "Annual Precipitation (mm)")
  ) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(1985, 2025, 5),  # Ajusta según tu rango
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  # COLORES PROFESIONALES (estilo de referencia)
  scale_color_manual(
    name = "Distance Zone",
    values = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#F0E442")
  ) +
  scale_fill_manual(
    name = "Distance Zone",
    values = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#F0E442")
  ) +
  
  # TEMA LIMPIO PARA PUBLICACIÓN
  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    # Ejes
    axis.line = element_line(linewidth = 0.5, color = "black"),
    axis.ticks = element_line(linewidth = 0.5, color = "black"),
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    
    # Eje derecho (precipitación)
    axis.title.y.right = element_text(color = "grey30", margin = margin(l = 10)),
    axis.text.y.right = element_text(color = "grey30"),
    axis.line.y.right = element_line(color = "grey30"),
    axis.ticks.y.right = element_line(color = "grey30"),
    
    # Leyenda
    legend.position = "bottom",
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "grey50", linewidth = 0.3),
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.1, "lines"),
    legend.margin = margin(4, 6, 4, 6),
    
    # Panel
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    # Márgenes
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave("output/figures/02.p_temporal.pdf", 
       width = 8, height = 5, dpi = 300, bg = "white")




## 2.5 Comparative maps by period ------------------------------------------
multiple_changes <- datos_finales %>%
  group_by(id) %>%
  summarise(
    coord_x = first(coord_x),
    coord_y = first(coord_y),
    distance_km = first(distance_km),
    # Period averages 
    ndvi_pre = mean(ndvi_mediana[year < 1991], na.rm = TRUE),
    ndvi_aoulouz = mean(ndvi_mediana[year >= 1991 & year <= 2002], na.rm = TRUE),
    ndvi_system = mean(ndvi_mediana[year > 2002], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    change_aoulouz = ndvi_aoulouz - ndvi_pre,
    change_system = ndvi_system - ndvi_pre,
    system_improvement = ndvi_system - ndvi_aoulouz
  ) %>%
  pivot_longer(
    cols = c(change_aoulouz, change_system, system_improvement),
    names_to = "change_type",
    values_to = "change"
  ) %>%
  mutate(
    change_type = case_when(
      change_type == "change_aoulouz" ~ "Aoulouz only\n(1991-02 vs Pre)",
      change_type == "change_system" ~ "Full system\n(2003-10 vs Pre)", 
      change_type == "system_improvement" ~ "System benefit\n(2003-10 vs 1991-02)"
    )
  ) %>% 
  mutate(
    transition = factor(change_type, levels = c(
      "Aoulouz only\n(1991-02 vs Pre)",
      "System benefit\n(2003-10 vs 1991-02)", 
      "Full system\n(2003-10 vs Pre)"
    )))


# Convert to sf
changes_sf_multiple <- multiple_changes %>%
  left_join(select(datos_geom, key_), by = c("id"="key_"))

# Map with facets
ggplot(changes_sf_multiple) +
  geom_sf(aes(fill = change, geometry=geom), size = 0.8) +
  facet_wrap(~change_type, ncol = 3) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "darkgreen", 
    midpoint = 0, name = "NDVI\nChange",
    limits = c(-0.05, 0.05)  # Adjust according to your data
  ) +
  labs(title = "Effect Comparison by Periods",
       subtitle = "Red = deterioration, Green = improvement") +
  theme_void() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

ggsave("output/06.data_vis/03.p_facet_maps.png")

## 2.6. 4 year window ------------------------------------------------------

detailed_changes <- datos_finales %>%
  group_by(id) %>%
  summarise(
    coord_x = first(coord_x),
    coord_y = first(coord_y),
    ndvi_pre_early = mean(ndvi_mediana[year >= 1984 & year <= 1986], na.rm = TRUE),  # Early pre
    ndvi_pre_late = mean(ndvi_mediana[year >= 1987 & year <= 1990], na.rm = TRUE),   # Late pre
    ndvi_aou_early = mean(ndvi_mediana[year >= 1991 & year <= 1994], na.rm = TRUE),
    ndvi_aou_mid = mean(ndvi_mediana[year >= 1995 & year <= 1998], na.rm = TRUE),
    ndvi_aou_late = mean(ndvi_mediana[year >= 1999 & year <= 2002], na.rm = TRUE),
    ndvi_sys_early = mean(ndvi_mediana[year >= 2003 & year <= 2006], na.rm = TRUE),
    ndvi_sys_late = mean(ndvi_mediana[year >= 2007 & year <= 2010], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    step_0 = ndvi_pre_late - ndvi_pre_early,     # Early pre → Late pre (natural evolution)
    step_1 = ndvi_aou_early - ndvi_pre_late,     # Late pre → Initial Aoulouz
    step_2 = ndvi_aou_mid - ndvi_aou_early,      # Initial → Mid Aoulouz
    step_3 = ndvi_aou_late - ndvi_aou_mid,       # Mid → Late Aoulouz  
    step_4 = ndvi_sys_early - ndvi_aou_late,     # Late Aoulouz → Initial system
    step_5 = ndvi_sys_late - ndvi_sys_early      # Initial → Late system
  ) %>%
  pivot_longer(
    cols = starts_with("step_"),
    names_to = "transition",
    values_to = "change"
  ) %>%
  mutate(
    transition = case_when(
      transition == "step_0" ~ "Natural evolution\n(1984-86 → 1987-90)", 
      transition == "step_1" ~ "Pre → Initial Aoulouz\n(1987-90 → 1991-94)", 
      transition == "step_2" ~ "Initial → Mid Aoulouz\n(1991-94 → 1995-98)", 
      transition == "step_3" ~ "Mid → Late Aoulouz\n(1995-98 → 1999-02)", 
      transition == "step_4" ~ "Aoulouz → System\n(1999-02 → 2003-06)", 
      transition == "step_5" ~ "Initial → Late system\n(2003-06 → 2007-10)" 
    )) %>% 
  mutate(
    transition = factor(transition, levels = c(
      "Natural evolution\n(1984-86 → 1987-90)", 
      "Pre → Initial Aoulouz\n(1987-90 → 1991-94)",
      "Initial → Mid Aoulouz\n(1991-94 → 1995-98)",
      "Mid → Late Aoulouz\n(1995-98 → 1999-02)", 
      "Aoulouz → System\n(1999-02 → 2003-06)", 
      "Initial → Late system\n(2003-06 → 2007-10)" 
    )))


# Convert to sf and map
detailed_changes_sf <- detailed_changes %>%
  left_join(select(datos_geom, key_), by = c("id"="key_"))

# Map with more periods
ggplot(detailed_changes_sf) +
  geom_sf(aes(fill = change, geometry = geom), size = 0.6) +
  facet_wrap(~transition, ncol = 3) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "darkgreen", 
    midpoint = 0, name = "NDVI\nChange"
  ) +
  labs(title = "Step-by-Step Evolution: Changes Between Consecutive Periods",
       subtitle = "Red = deterioration, Green = improvement") +
  theme_void() +
  theme(
    strip.text = element_text(size = 9, face = "bold"),  # Slightly smaller text for 6 maps
    legend.position = "bottom"
  )

ggsave("output/06.data_vis/06.p_detailed_maps.png")



## 2.7 Comparing years -----------------------------------------------------

library(ggpattern)  # Necesitas instalar: install.packages("ggpattern")

# 1. Preparar los datos - calcular promedios mensuales por período
datos_plot <- datos_finales %>%
  mutate(period = ifelse(year < 1991, "Pre", "Post")) %>%
  group_by(month, period) %>%
  summarise(mean_ndvi = max(ndvi_mediana, na.rm = TRUE),
            .groups = "drop")

# 2. Definir las estaciones - separar Dry Season del resto
estaciones_base <- data.frame(
  xmin = c(1, 11, 6),        
  xmax = c(5, 12, 10),       
  ymin = c(-Inf, -Inf, -Inf),
  ymax = c(Inf, Inf, Inf),
  station = c("Season 1", "Season 1", "Season 2"),
  fill_color = c("#F0E442", "#F0E442", "#56B4E9")
)

# Dry Season como rectángulo separado para el patrón
dry_season <- data.frame(
  xmin = 5,
  xmax = 8,
  ymin = -Inf,
  ymax = Inf
)

# 3. Crear el gráfico
ggplot(datos_plot, aes(x = month, y = mean_ndvi)) +
  
  # Rectángulos de fondo para Season 1 y Season 2 (sin patrón)
  geom_rect(data = estaciones_base,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = station),
            inherit.aes = FALSE, alpha = 0.2) +
  
  #Dry Season con patrón rayado
  geom_rect_pattern(data = dry_season,
                    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                    inherit.aes = FALSE,
                    fill = "#999999",
                    alpha = 0.05,
                    pattern = "stripe",
                    pattern_density = 0.05,
                    pattern_spacing = 0.02,
                    pattern_fill = "#666666",
                    pattern_alpha = 0.1,
                    pattern_angle = 45) +
  
  # Líneas ESCALONADAS de NDVI
  geom_step(data = filter(datos_plot, period == "Pre"),
            aes(color = "Pre"), linewidth = 0.8) +
  geom_step(data = filter(datos_plot, period == "Post"),
            aes(color = "Post"), linewidth = 0.8) +
  
  # Etiquetas de estaciones
  annotate("text", x = 3, y = Inf, label = "Season 1 (Nov–May)", 
           vjust = 1.5, size = 3, color = "grey40") +
  annotate("text", x = 8, y = Inf, label = "Season 2 (Jun–Oct)", 
           vjust = 1.5, size = 3, color = "grey40") +
  annotate("text", x = 6.5, y = Inf, label = "Dry Season\n(May–Aug)", 
           vjust = 3.5, size = 2.5, color = "grey30", fontface = "bold") +
  
  # Escalas
  scale_x_continuous(
    name = "Month",
    breaks = 1:12,
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name = "NDVI",
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  # Colores profesionales
  scale_color_manual(
    name = "Period",
    values = c("Pre" = "#0072B2", "Post" = "#D55E00")
  ) +
  scale_fill_manual(
    name = "Season",
    values = c("Season 1" = "#F0E442", 
               "Season 2" = "#56B4E9"),
    guide = "none"
  ) +
  
  # Tema limpio para publicación
  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    # Ejes
    axis.line = element_line(linewidth = 0.5, color = "black"),
    axis.ticks = element_line(linewidth = 0.5, color = "black"),
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    
    # Leyenda
    legend.position = c(0.02, 0.3),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "grey50", linewidth = 0.3),
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.1, "lines"),
    legend.margin = margin(4, 6, 4, 6),
    
    # Panel
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    # Márgenes
    plot.margin = margin(10, 15, 10, 10)
  )

# 4. Guardar
ggsave("output/figures/03.ndvi_monthly_pre_post.pdf", 
       width = 7, height = 4, dpi = 300, bg = "white")


# Missing -------------------------------------------------------

years <- 1984:1997
months <- 5:7
complete_grid <- expand_grid(year = years, month = months)

availability <- datos_finales %>%
  group_by(year, month) %>%
  summarise(n_observations = n(), .groups = "drop")

image_availability <- complete_grid %>%
  left_join(availability, by = c("year", "month")) %>%
  mutate(
    has_image = !is.na(n_observations),
    n_observations = replace_na(n_observations, 0)
  )

# Graph
ggplot(image_availability, aes(x = factor(month), y = factor(year))) +
  geom_tile(aes(fill = has_image), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("TRUE" = "#52C4A0", "FALSE" = "#E74C3C"),
    labels = c("TRUE" = "True", "FALSE" = "False"),
    name = "Has Image"
  ) +
  scale_x_discrete(labels = month.abb[5:7]) +
  labs(
    title = "Availability of Imagery Over Time",
    x = "Month",
    y = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0),
    panel.grid = element_blank(),
    legend.position = "right"
  )
ggsave("output/06.data_vis/07.availability.png")



# 3. Number of dams - FAO ----------------------------------------------------


library(tidyverse)
library(scales)

dams <- readxl::read_xlsx("data/Africa-dams_eng.xlsx", sheet = 2, skip = 1) %>% 
  filter(Country == "Morocco") 

all_dams <- dams %>% 
  filter(`Completed /operational since` != "Incomplete?") %>% 
  mutate(`Completed /operational since` = as.numeric(`Completed /operational since`)) %>% 
  group_by(`Completed /operational since`) %>% 
  summarise(total = n(), 
            capacity_m3_mean = mean(`Reservoir capacity (million m3)`,na.rm=T),
            capacity_m3_total = sum(`Reservoir capacity (million m3)`,na.rm=T)) %>% 
  mutate(cumulative_n = cumsum(total),
         cumulative_m3 = cumsum(capacity_m3_total))
  
irrigation <- dams %>% 
  filter(`Completed /operational since` != "Incomplete?") %>% 
  mutate(`Completed /operational since` = as.numeric(`Completed /operational since`)) %>%
  filter(Irrigation == "x") %>% 
  group_by(`Completed /operational since`) %>% 
  summarise(total = n()) %>% 
  mutate(cumulative_irrigation_n = cumsum(total)) %>% 
  select(-total)

dams_mor <- left_join(all_dams, irrigation,
                      by="Completed /operational since") %>% 
  fill(cumulative_irrigation_n, .direction = "down")


factor_escala <- max(dams_mor$cumulative_n, na.rm = TRUE) / 
  max(dams_mor$cumulative_m3, na.rm = TRUE)

eventos <- data.frame(
  year = c(1956, 1980, 2008),
  label = c("Independence\n(1956)",
            "Liberalization\n(1980s)", 
            "Green Morocco\nPlan (2008)")
)

ggplot(dams_mor, aes(x = `Completed /operational since`)) +
  # eventos
  geom_vline(data = eventos, aes(xintercept = year),
             linetype = "dashed", color = "grey30", linewidth = 0.5, alpha = 0.7) +
  
  # Área primero (fondo)
  geom_area(aes(y = cumulative_m3 * factor_escala, fill = "Cumulative volume"), 
            alpha = 0.2) +
  # Líneas encima
  geom_line(aes(y = cumulative_n, color = "Total dams"), 
            linewidth = 0.8) +
  geom_line(aes(y = cumulative_irrigation_n, color = "Irrigation dams"), 
            linewidth = 0.8) +
  # etiquetas evento
  geom_text(data = eventos, 
            aes(x = year, y = Inf, label = label),
            vjust = 1.2, hjust = 0.5, size = 2.5, 
            color = "grey20", lineheight = 0.9) +
  
  # Escalas
  scale_y_continuous(
    name = "Cumulative number of dams",
    sec.axis = sec_axis(~ . / factor_escala, 
                        name = "M³")
  ) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(1920, 2020, 20),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  # Colores profesionales
  scale_color_manual(
    name = NULL,
    values = c("Total dams" = "#0072B2", 
               "Irrigation dams" = "#D55E00")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Cumulative volume" = "grey40")
  ) +
  # Tema limpio para publicación
  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    # Ejes
    axis.line = element_line(linewidth = 0.5, color = "black"),
    axis.ticks = element_line(linewidth = 0.5, color = "black"),
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    
    # Eje derecho
    axis.title.y.right = element_text(color = "grey30", margin = margin(l = 10)),
    axis.text.y.right = element_text(color = "grey30"),
    axis.line.y.right = element_line(color = "grey30"),
    axis.ticks.y.right = element_line(color = "grey30"),
    
    # Leyenda
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "grey50", linewidth = 0.3),
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.1, "lines"),
    legend.margin = margin(4, 6, 4, 6),
    
    # Panel
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    # Márgenes
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave("output/figures/01.dam_evolution.pdf", 
       width = 7, height = 4, dpi = 300, bg = "white")

rm(dams, all_dams, irrigation, dams_mor , factor_escala,
   eventos)


# 4. maps aspect, elevation, slope-----
library(sf)

maps_topo <- datos_geom %>% 
  left_join(st_drop_geometry(topo), by = c("key_"))


theme_map_pub <- function() {
  theme_classic(base_size = 11, base_family = "sans") +
    theme(
      # Ejes
      axis.line = element_line(linewidth = 0.5, color = "black"),
      axis.ticks = element_line(linewidth = 0.5, color = "black"),
      axis.title = element_text(size = 10, color = "black"),
      axis.text = element_text(size = 9, color = "black"),
      
      # Leyenda
      legend.position = "right",
      legend.background = element_rect(fill = "white", color = "grey50", linewidth = 0.3),
      legend.key.size = unit(0.8, "lines"),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.margin = margin(4, 6, 4, 6),
      
      # Panel
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      
      # Márgenes
      plot.margin = margin(10, 15, 10, 10),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5)
    )
}

# Paletas profesionales
pal_elevation <- scale_fill_viridis_c(option = "mako", direction = -1, name = "Elevation (m)")
pal_slope <- scale_fill_viridis_c(option = "rocket", direction = -1, name = "Slope (°)")
pal_aspect <- scale_fill_gradientn(
  colors = c("#0072B2", "#56B4E9", "#F0E442", "#D55E00", "#0072B2"),  # circular
  name = "Aspect (°)",
  breaks = c(0, 90, 180, 270, 360)
)

# Mapas
p_elev <- ggplot(maps_topo) +
  geom_sf(aes(fill = elevation), color = NA) +  # ajusta "elevation" al nombre real
  pal_elevation +
  #labs(title = "A) Elevation") +
  #theme_map_pub()
  theme_void()

p_slope <- ggplot(maps_topo) +
  geom_sf(aes(fill = slope), color = NA) +
  pal_slope +
  #labs(title = "B) Slope") +
  #theme_map_pub()
  theme_void()

p_aspect <- ggplot(maps_topo) +
  geom_sf(aes(fill = aspect), color = NA) +
  pal_aspect +
  #labs(title = "C) Aspect") +
  theme_void()

ggsave(plot = p_elev, "output/figures/02.topography_maps_elev.pdf", 
       #width = 12, 
       height = 4, dpi = 300, bg = "white")
ggsave(plot = p_slope, "output/figures/02.topography_maps_slope.pdf", 
       #width = 12,
       height = 4, dpi = 300, bg = "white")
ggsave(plot = p_aspect,"output/figures/02.topography_maps_aspect.pdf", 
       #width = 12,
       height = 4, dpi = 300, bg = "white")







