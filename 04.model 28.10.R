# 0. Libraries ------------------------------------------------------------

library(tidyverse)
library(fixest)
library(broom)
library(patchwork)
library(purrr)
library(stringr)
library(modelsummary)
library(stargazer)
library(sf)
library(conleyreg)

rm(list = ls())
gc()


# 1. Data -----------------------------------------------------------------

#load("data/data_prep_plot_lv.RData")
load("data/workspace_plot_lv.RData")


# 2. Función para crear donut --------------------------------------------

crear_donut <- function(df, inner, outer_min, outer_max) {
  df %>%
    mutate(
      donut_sample = case_when(
        distance_km < inner ~ "excluded_inner",
        distance_km >= inner & distance_km < outer_min ~ "treated",
        distance_km >= outer_min & distance_km <= outer_max ~ "excluded_middle", 
        distance_km > outer_max ~ "control",
        TRUE ~ "other"
      ),
      donut_treat = case_when(
        donut_sample == "treated" ~ 1,
        donut_sample == "control" ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(donut_treat)) %>%
    mutate(donut_treat_post = donut_treat * post)
}


# 3. Parámetros -----------------------------------------------------------

inner_hole <- 1
outer_hole_min <- 5
outer_hole_max <- 10


# 4. TEMPORADA DRY --------------------------------------------------------

df_dry_donut <- crear_donut(df_dry, inner_hole, outer_hole_min, outer_hole_max)
df_dry$id_num <- as.numeric(as.factor(df_dry$id))

dry_did1 <- feols(max_ndvi ~ treat_post, data = df_dry)
dry_did2 <- feols(max_ndvi ~ treat_post | id + year, data = df_dry, cluster = "block")
dry_donut <- feols(max_ndvi ~ donut_treat_post | id + year, data = df_dry_donut, cluster = "block")

dry_conley1 <- conleyreg(max_ndvi ~ treat_post, data = df_dry,
                         lat = "coord_y", lon = "coord_x", dist_cutoff = 5,
                         kernel = "bartlett", crs = 26192, 
                         unit = "id_num", time = "year")

dry_conley2 <- conleyreg(max_ndvi ~ treat_post | id + year, data = df_dry,
                         lat = "coord_y", lon = "coord_x", dist_cutoff = 5,
                         kernel = "bartlett", crs = 26192, 
                         unit = "id_num", time = "year")


# 5. TEMPORADA SEASON_1 ---------------------------------------------------

df_season_1_donut <- crear_donut(df_season_1, inner_hole, outer_hole_min, outer_hole_max)
df_season_1$id_num <- as.numeric(as.factor(df_season_1$id))

s1_did1 <- feols(max_ndvi ~ treat_post, data = df_season_1)
s1_did2 <- feols(max_ndvi ~ treat_post | id + grow_year_season, data = df_season_1, cluster = "block")
s1_donut <- feols(max_ndvi ~ donut_treat_post | id + grow_year_season, data = df_season_1_donut, cluster = "block")

s1_conley1 <- conleyreg(max_ndvi ~ treat_post, data = df_season_1,
                        lat = "coord_y", lon = "coord_x", dist_cutoff = 5,
                        kernel = "bartlett", crs = 26192, 
                        unit = "id_num", time = "grow_year_season")

s1_conley2 <- conleyreg(max_ndvi ~ treat_post | id + grow_year_season, data = df_season_1,
                        lat = "coord_y", lon = "coord_x", dist_cutoff = 5,
                        kernel = "bartlett", crs = 26192, 
                        unit = "id_num", time = "grow_year_season")


# 6. TEMPORADA SEASON_2 ---------------------------------------------------

df_season_2_donut <- crear_donut(df_season_2, inner_hole, outer_hole_min, outer_hole_max)
df_season_2$id_num <- as.numeric(as.factor(df_season_2$id))

s2_did1 <- feols(max_ndvi ~ treat_post, data = df_season_2)
s2_did2 <- feols(max_ndvi ~ treat_post | id + grow_year_season, data = df_season_2, cluster = "block")
s2_donut <- feols(max_ndvi ~ donut_treat_post | id + grow_year_season, data = df_season_2_donut, cluster = "block")

s2_conley1 <- conleyreg(max_ndvi ~ treat_post, data = df_season_2,
                        lat = "coord_y", lon = "coord_x", dist_cutoff = 5,
                        kernel = "bartlett", crs = 26192, 
                        unit = "id_num", time = "grow_year_season")

s2_conley2 <- conleyreg(max_ndvi ~ treat_post | id + grow_year_season, data = df_season_2,
                        lat = "coord_y", lon = "coord_x", dist_cutoff = 5,
                        kernel = "bartlett", crs = 26192, 
                        unit = "id_num", time = "grow_year_season")



# 7. TABLA FINAL CON SE DE CONLEY ----------------------------------------

models_list <- list(
  "Dry" = list(
    "(I)" = dry_did1, "(II)" = dry_did2, "(III)" = dry_donut
  ),
  "Season 1" = list(
    "(IV)" = s1_did1, "(V)" = s1_did2, "(VI)" = s1_donut
  ),
  "Season 2" = list(
    "(VII)" = s2_did1, "(VIII)" = s2_did2, "(IX)" = s2_donut
  )
)

## Extraer SE de Conley (segunda columna de la matriz)
conley_se <- list(
  dry_conley1["treat_post", "Std. Error"],
  dry_conley2["treat_post", "Std. Error"],
  NA,
  s1_conley1["treat_post", "Std. Error"],
  s1_conley2["treat_post", "Std. Error"],
  NA,
  s2_conley1["treat_post", "Std. Error"],
  s2_conley2["treat_post", "Std. Error"],
  NA
)

# Crear fila para SE Conley
conley_row <- data.frame(
  term = "",
  `Dry (1)` = paste0("{", round(conley_se[[1]], 4), "}"),
  `Dry (2)` = paste0("{", round(conley_se[[2]], 4), "}"),
  `Dry (3)` = "",
  `S1 (1)` = paste0("{", round(conley_se[[4]], 4), "}"),
  `S1 (2)` = paste0("{", round(conley_se[[5]], 4), "}"),
  `S1 (3)` = "",
  `S2 (1)` = paste0("{", round(conley_se[[7]], 4), "}"),
  `S2 (2)` = paste0("{", round(conley_se[[8]], 4), "}"),
  `S2 (3)` = "",
  check.names = FALSE
)

attr(conley_row, 'position') <- c(3,9)

mod_1 <- modelsummary(
  models_list,
  stars = TRUE,
  statistic = c("({std.error})"),   
  add_rows = conley_row,
  coef_omit = "Intercept",
  gof_omit = 'AIC|BIC|RMSE|Adj|Log|Within Adj',
  coef_rename = c("treat_post"= "Spec",
                  "donut_treat_post" = "Donut"),
  shape = "cbind",
  output = "latex_tabular"
)

writeLines(as.character(mod_1), "output/model/model_comp_final.tex")



#8. Distance bins & gradient ------------------------------------------------

rm(list = ls())
gc()
load("data/workspace_plot_lv.RData")


distance_dry <- feols(max_ndvi ~ i(distance_bin, post, ref = "15+km") | id + year,
                     data = df_dry, cluster = "block")

distance_s1 <- feols(max_ndvi ~ i(distance_bin, post, ref = "15+km") | id + grow_year_season,
                        data = df_season_1, cluster = "block")

distance_s2 <- feols(max_ndvi ~ i(distance_bin, post, ref = "15+km") | id + grow_year_season,
                     data = df_season_2, cluster = "block")

modelsummary(models =  list("Dry" = distance_dry,
                            "S1" = distance_s1,
                            "S2" = distance_s2),
             stars = T,
             #output = "docx",
             #output = "output/model/gradient.docx"
             )

#latex
mod2 <- modelsummary(
  models = list("Dry" = distance_dry,
                "Season 1" = distance_s1,
                "Season 2" = distance_s2),
  stars = TRUE,
  gof_omit = 'AIC|BIC|RMSE|Adj|Log|Within Adj',
  coef_rename = c("distance_bin::0-3km:post" = "0-3km × Post", 
                  "distance_bin::3-6km:post" = "3-6km × Post", 
                  "distance_bin::6-9km:post" = "6-9km × Post", 
                  "distance_bin::9-12km:post" = "9-12km × Post", 
                  "distance_bin::12-15km:post" = "12-15km × Post"),
  output = "latex_tabular"
)

writeLines(as.character(mod2), "output/model/gradient.tex")


# Distance gradient loop
distance_breaks <- seq(2, 17, by = 1)
distance_results <- map_dfr(distance_breaks, function(d) {
  df_temp <- df_season_1 %>%
    mutate(near_temp = ifelse(distance_km <= d, 1, 0),
           near_post_temp = near_temp * post)
  
  m_temp <- feols(max_ndvi ~ near_post_temp | id + grow_year_season,
                  data = df_temp, cluster = "block")
  tibble(
    distance = d,
    coefficient = coef(m_temp)["near_post_temp"],
    se = se(m_temp)["near_post_temp"]
  )
})

# 9. Plots ------------------------------------------------


plot_distance_bins <- function(model, title, var_c = "NDVI") {
  
  coefs_df <- broom::tidy(model, conf.int = TRUE) %>%
    dplyr::filter(term %in% c(
      "distance_bin::0-3km:post",
      "distance_bin::3-6km:post",
      "distance_bin::6-9km:post",
      "distance_bin::9-12km:post",
      "distance_bin::12-15km:post"
    )) %>%
    dplyr::mutate(
      term = factor(term,
                    levels = c(
                      "distance_bin::0-3km:post",
                      "distance_bin::3-6km:post",
                      "distance_bin::6-9km:post",
                      "distance_bin::9-12km:post",
                      "distance_bin::12-15km:post"
                    ),
                    labels = c("0-3 km", "3-6 km", "6-9 km", "9-12 km", "12-15 km")
      )
    )
  
  ggplot(coefs_df, aes(x = term, y = estimate)) +
    # Línea de referencia en cero
    geom_hline(yintercept = 0, linetype = "dashed", 
               color = "grey30", linewidth = 0.5, alpha = 0.7) +
    
    # Barras de error primero
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  width = 0.25, color = "#0072B2", linewidth = 0.6) +
    
    # Puntos encima
    geom_point(size = 3, color = "#D55E00") +
    
    # Etiquetas
    labs(
      title = title,
      x = "Distance bin",
      y = paste0("Change in ", var_c)
    ) +
    
    # Tema limpio para publicación
    theme_classic(base_size = 11, base_family = "sans") +
    theme(
      # Ejes
      axis.line = element_line(linewidth = 0.5, color = "black"),
      axis.ticks = element_line(linewidth = 0.5, color = "black"),
      axis.title = element_text(size = 10, color = "black"),
      axis.text = element_text(size = 9, color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      
      # Títulos
      plot.title = element_text(size = 11, face = "bold", color = "black"),
      plot.subtitle = element_text(size = 9, color = "grey30", 
                                   margin = margin(b = 10)),
      
      # Panel
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      # Márgenes
      plot.margin = margin(10, 15, 10, 10)
    )
}


plot_distance_gradient <- function(data, fixed_effects, title) {
  
  distance_breaks <- seq(2, 17, by = 1)
  
  distance_results <- purrr::map_dfr(distance_breaks, function(d) {
    df_temp <- data %>%
      dplyr::mutate(
        near_temp = as.integer(distance_km <= d),
        near_post_temp = near_temp * post
      )
    
    fe_formula <- as.formula(
      paste("max_ndvi ~ near_post_temp |", fixed_effects)
    )
    
    m_temp <- fixest::feols(
      fe_formula,
      data = df_temp,
      cluster = "block"
    )
    
    # Coeficiente seguro:
    coef_val <- suppressWarnings(
      tryCatch(coef(m_temp)["near_post_temp"], error = function(e) NA_real_)
    )
    
    # Intervalo seguro:
    ci_vals <- suppressWarnings(
      tryCatch(confint(m_temp, "near_post_temp"), error = function(e) c(NA_real_, NA_real_))
    )
    
    # Forzamos tipo numerico y nunca NULL
    conf_low <- ifelse(is.null(ci_vals[1]), NA_real_, ci_vals[1])
    conf_high <- ifelse(is.null(ci_vals[2]), NA_real_, ci_vals[2])
    
    tibble::tibble(
      distance = d,
      coefficient = as.numeric(coef_val),
      conf_low = as.numeric(conf_low),
      conf_high = as.numeric(conf_high)
    )
  }) %>%
    dplyr::filter(!is.na(coefficient))
  
  ggplot(distance_results, aes(x = distance, y = coefficient)) +
    # Líneas de referencia primero (fondo)
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey30", linewidth = 0.5, alpha = 0.7) +
    geom_vline(xintercept = 10, 
               linetype = "dashed", color = "#D55E00",
               linewidth = 0.5, alpha = 0.7) +
    
    # Intervalo de confianza (área)
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
                alpha = 0.2, fill = "#0072B2") +
    
    # Línea principal encima
    geom_line(color = "#0072B2", linewidth = 0.8) +
    
    # Etiquetas
    labs(
      title = title,
      
      x = "Distance threshold (km)",
      y = "DiD coefficient"
    ) +
    
    # Escalas
    scale_x_continuous(
      breaks = seq(2, 17, by = 3),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    
    # Tema limpio 
    theme_classic(base_size = 11, base_family = "sans") +
    theme(
      # Ejes
      axis.line = element_line(linewidth = 0.5, color = "black"),
      axis.ticks = element_line(linewidth = 0.5, color = "black"),
      axis.title = element_text(size = 10, color = "black"),
      axis.text = element_text(size = 9, color = "black"),
      
      # Títulos
      plot.title = element_text(size = 11, face = "bold", color = "black"),
      
      # Panel
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      
      # Márgenes
      plot.margin = margin(10, 15, 10, 10)
    )
}

# Gráficos por bins

plot_distance_bins(distance_dry, "Dry Season")
ggsave("output/figures/distance_bins_DRY.pdf", 
      width = 7, height = 4, dpi = 300, bg = "white")

plot_distance_bins(distance_s1, "Season 1")
ggsave("output/figures/distance_bins_S1.pdf", 
       width = 7, height = 4, dpi = 300, bg = "white")

plot_distance_bins(distance_s2, "Season 2")
ggsave("output/figures/distance_bins_S2.pdf", 
       width = 7, height = 4, dpi = 300, bg = "white")


# Did distance

# subtitle = "Estimated DiD coefficients across spatial thresholds",
plot_distance_gradient(df_dry, "id + year", "Dry Season")
ggsave("output/figures/distance_gradient_DID_dry.pdf", 
        width = 7, height = 4, dpi = 300, bg = "white")

plot_distance_gradient(df_season_1, "id + grow_year_season", "Season 1")
ggsave("output/figures/distance_gradient_DID_s1.pdf", 
       width = 7, height = 4, dpi = 300, bg = "white")

plot_distance_gradient(df_season_2, "id + grow_year_season", "Season 2")
ggsave("output/figures/distance_gradient_DID_s2.pdf", 
       width = 7, height = 4, dpi = 300, bg = "white")




# 10. Event study -------------------------------------------------------------

event_model <- feols(max_ndvi ~ i(event_time, treat, ref = -1) | id + year,
                     data = df_dry, cluster = "block")

iplot(event_model)

# Para el paper - 
coef_names <- names(coef(event_model))
coef_vals <- coef(event_model)
se_vals <- se(event_model)

# Filtrar solo los de event_time
event_idx <- grepl("event_time", coef_names)
coefs <- data.frame(
  term = coef_names[event_idx],
  estimate = coef_vals[event_idx],
  std.error = se_vals[event_idx]
) %>%
  mutate(
    event_time = as.numeric(gsub(".*event_time::(-?[0-9]+):treat.*", "\\1", term)),
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Añadir punto de referencia (ref = -1)
ref_point <- data.frame(
  term = "event_time::-1:treat",
  estimate = 0,
  std.error = 0,
  event_time = -1,
  conf.low = 0,
  conf.high = 0
)

coefs <- bind_rows(coefs, ref_point) %>%
  arrange(event_time)


ggplot(coefs, aes(x = event_time, y = estimate)) +
  # LÍNEA DE REFERENCIA en 0
  geom_hline(yintercept = 0, 
             linetype = "solid", color = "black", linewidth = 0.3) +
  # LÍNEA VERTICAL en event time = 0
  geom_vline(xintercept = -0.5,  # Entre -1 y 0
             linetype = "dashed", color = "grey30", linewidth = 0.5, alpha = 0.7) +
  # ETIQUETA DE EVENTO
  annotate("text", x = -0.5, y = Inf, label = "Dam\ncompletion", 
           vjust = 1.2, hjust = 0.5, size = 2.5, 
           color = "grey20", lineheight = 0.9) +
  # INTERVALO DE CONFIANZA (ribbon)
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = "#0072B2", alpha = 0.2) +
  # LÍNEA DE COEFICIENTES
  geom_line(color = "#0072B2", linewidth = 0.8) +
  # PUNTOS
  geom_point(color = "#0072B2", size = 2.5) +
  # ESCALAS
  scale_x_continuous(
    name = "Years relative to dam completion",
    breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)
  ) +
  scale_y_continuous(
    name = "Treatment effect on NDVI"
  ) +
  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    axis.line = element_line(linewidth = 0.5, color = "black"),
    axis.ticks = element_line(linewidth = 0.5, color = "black"),
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave("output/figures/04.event_study.pdf", 
       width = 7, height = 4, dpi = 300, bg = "white")

# 10. Mechanism ------------------------------------
## 10.1 Rain ------------------------------------

# 1. ESTANDARIZAR PRECIPITACIÓN

df_dry <- df_dry %>% 
  mutate(sum_rain_std = (sum_rain - mean(sum_rain, na.rm = TRUE)) / sd(sum_rain, na.rm = TRUE))

df_season_1 <- df_season_1 %>% 
  mutate(sum_rain_std = (sum_rain - mean(sum_rain, na.rm = TRUE)) / sd(sum_rain, na.rm = TRUE))

df_season_2 <- df_season_2 %>% 
  mutate(sum_rain_std = (sum_rain - mean(sum_rain, na.rm = TRUE)) / sd(sum_rain, na.rm = TRUE))



# 2. MODELOS DE ELASTICIDAD 

mech_dry <- feols(
  max_ndvi ~ treat*post*sum_rain_std | year,
  data = df_dry,
  cluster = "block"
)

mech_s1 <- feols(
  max_ndvi ~ treat*post*sum_rain_std | grow_year_season,
  data = df_season_1,
  cluster = "block"
)

mech_s2 <- feols(
  max_ndvi ~ treat*post*sum_rain_std | grow_year_season,
  data = df_season_2,
  cluster = "block"
)

# 3. TABLA DE RESULTADOS

mod3 <- modelsummary(
  models = list(
    "Dry (May-Aug)" = mech_dry,
    "Season 1 (Nov-May)" = mech_s1,
    "Season 2 (Jun-Oct)" = mech_s2
  ),
  stars = TRUE,
  coef_rename = c(
    "treat" = "Treated",
    #"post" = "Post",
    "sum_rain_std" = "Precipitation (std)",
    "treat:post" = "Treated × Post",
    "treat:sum_rain_std" = "Treated × Precipitation",
    "post:sum_rain_std" = "Post × Precipitation",
    "treat:post:sum_rain_std" = "Treated × Post × Precipitation"
  ),
  gof_omit = 'AIC|BIC|RMSE|Adj|Log|Within Adj', 
  output = "latex_tabular"
  #output = "output/model/mech1_rain.docx",
)

writeLines(as.character(mod3), "output/model/mech1_rain.tex")


## 10.2 water access -----

# DID básico
ndwi_dry <- feols(max_ndwi ~ treat_post | id + year, 
                  data = df_dry, cluster = "block")

ndwi_s1 <- feols(max_ndwi ~ treat_post | id + grow_year_season, 
                 data = df_season_1, cluster = "block")

ndwi_s2 <- feols(max_ndwi ~ treat_post | id + grow_year_season, 
                 data = df_season_2, cluster = "block")



mod_3 <- modelsummary(models =  list("Dry" = ndwi_dry,
                            "Season 1" = ndwi_s1,
                            "Season 2" = ndwi_s2),
             stars = T,
             gof_omit = 'AIC|BIC|RMSE|Adj|Log|Within Adj',
             coef_rename = c("treat_post"="High Exposure × Post-dam"),
             output = "latex_tabular",
             #output = "output/model/mech2_water.docx"
             )

writeLines(as.character(mod_3), "output/model/mech2_water.tex")

## 10.2.1 NDWI -----------------------------------------------------------------------------


ndwi_bins_dry <- feols(max_ndwi ~ i(distance_bin, post, ref = "15+km") | id + year,
                       data = df_dry, cluster = "block")

ndwi_bins_s1 <- feols(max_ndwi ~ i(distance_bin, post, ref = "15+km") | id + grow_year_season,
                      data = df_season_1, cluster = "block")

ndwi_bins_s2 <- feols(max_ndwi ~ i(distance_bin, post, ref = "15+km") | id + grow_year_season,
                      data = df_season_2, cluster = "block")


modelsummary(
  list("Dry\n(May-Aug)" = ndwi_bins_dry, 
       "Season 1\n(Nov-May)" = ndwi_bins_s1, 
       "Season 2\n(Jun-Oct)" = ndwi_bins_s2),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c("nobs", "r.squared"),
  coef_rename = c(
    "post:distance_bin0-3km" = "0-3km × Post",
    "post:distance_bin3-6km" = "3-6km × Post",
    "post:distance_bin6-9km" = "6-9km × Post",
    "post:distance_bin9-12km" = "9-12km × Post",
    "post:distance_bin12-15km" = "12-15km × Post"
  ),
  title = "Heterogeneous Effects on Water Availability by Distance"
)



plot_distance_bins(ndwi_bins_dry, "Dry Season", var_c = "NDWI")
ggsave("output/figures/mech2_distance_bins_DRY.pdf", 
       width = 7, height = 4, dpi = 300, bg = "white")

plot_distance_bins(ndwi_bins_s1, "Season 1", var_c = "NDWI")
ggsave("output/figures/mech2_distance_bins_s1.pdf", 
       width = 7, height = 4, dpi = 300, bg = "white")

plot_distance_bins(ndwi_bins_s2, "Season 2", var_c = "NDWI")
ggsave("output/figures/mech2_distance_bins_S2.pdf", 
       width = 7, height = 4, dpi = 300, bg = "white")


## 10.2.3 Other models --------------
### 10.2.3.1 Model with area ---------------------------------------------------------

load("data/workspace_plot_lv.RData")

df_season_2 <- left_join(df_season_2, topo, by = c("id"="key_"))

feols(max_ndvi ~ i(distance_bin, post, ref = "15+km") + 
        area:post | id + grow_year_season,
      data = df_season_2, cluster = "block")

### 10.2.3.1 by WUA -----------------------------------------------------------

df_season_2<- df_season_2 %>% 
  mutate(sector = substr(id, start = 0, 2),
         area_cat_2  = factor(case_when(
           area < 5000 ~ "Less than 0.5ha",
           area >= 5000 & area < 10000 ~ "Between 0.5 & 1 ha",
           area >= 10000 & area < 20000 ~ "Between 1 & 2 ha",
           area >= 20000  ~ "More than 2 ha" 
           ),levels = c("Less than 0.5ha","Between 0.5 & 1 ha",
                        "Between 1 & 2 ha","More than 2 ha" ),
           labels = c("Less than 0.5ha","Between 0.5 & 1 ha",
                      "Between 1 & 2 ha","More than 2 ha" ))) %>% 
  mutate(slope_category = factor(slope_category))


# Lista de regiones tal como están en la variable AUEA
lista_auea <- unique(df_season_2)  

# Lista para guardar los modelos
modelos_por_auea <- list()

# Loop para ajustar un modelo por cada región
for (reg in lista_auea) {
  
  # Filtramos datos para esa AUEA
  df_reg <- subset(df_season_2, sector == reg)
  
  # Estimamos el modelo
  mod <- feols(max_ndvi ~ post * slope_category + distance_km | id + grow_year_season,
               data = df_reg,
               cluster = "id")  # Cambia por "block" si ese es tu agrupador estándar
  
  # Guardamos el modelo en la lista con nombre de región
  modelos_por_auea[[reg]] <- mod
}

# Mostrar todos los modelos juntos
modelsummary(modelos_por_auea,
             stars = TRUE,
             gof_omit = "AIC|BIC|Log.Lik|F|R2",  # opcional, limpia medidas innecesarias
             #output = "output/model/auea_by_area.docx"
             ) 


# Calcular medias NDVI por region, tamaño y periodo
ndvi_summary <- df_season_2 %>%
  group_by(sector, area_cat_2, post) %>%
  summarise(mean_ndvi = mean(max_ndvi, na.rm = TRUE),
            n = n()) %>%
  ungroup()

# Convertir post a etiqueta clara
ndvi_summary$post_label <- ifelse(ndvi_summary$post == 0, "Antes", "Después")

# Graficar
ggplot(ndvi_summary, aes(x = area_cat_2, y = mean_ndvi,
                         fill = post_label)) +
  geom_col(position = position_dodge(.8), width = 0.7) +
  facet_wrap(~ sector) +
  scale_fill_manual(values = c("Antes" = "#1f77b4", "Después" = "#ff7f0e")) +
  labs(title = "NDVI medio antes y después de la represa por tamaño de plot y región",
       x = "Categoría de área",
       y = "NDVI medio",
       fill = "Periodo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


mod_triple <- feols(max_ndvi ~ post * area_cat_2 * sector | id + grow_year_season,
                    data = df_season_2,
                    cluster = "id")
summary(mod_triple)



ggplot(df_season_2, aes(x = grow_year_season, y = max_ndvi,
                        color = area_cat_2, group = area_cat_2)) +
  stat_summary(fun = mean, geom = "line") +
  facet_wrap(~ sector) +
  geom_vline(xintercept = 1991, linetype = "dashed", color = "red") +
  labs(title = "Evolución NDVI por tamaño de plot y región",
       x = "Año agrícola", y = "NDVI medio") +
  theme_minimal()
