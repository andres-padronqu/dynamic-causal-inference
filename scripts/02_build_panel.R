# =========================================================
# 02_build_panel.R
# Construcción de panel entidad-periodo
# Proyecto:
# Dynamic Causal Inference - Guardia Nacional
# =========================================================

# ---------------------------------------------------------
# Librerías
# ---------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)

# ---------------------------------------------------------
# Configuración
# ---------------------------------------------------------

set.seed(1234)

options(scipen = 999)

# ---------------------------------------------------------
# Paths
# ---------------------------------------------------------

path_processed <- "data/processed"

# ---------------------------------------------------------
# Cargar datos limpios
# ---------------------------------------------------------

timeseries_clean <- read_csv(
  file.path(path_processed, "timeseries_clean.csv"),
  show_col_types = FALSE
)

# ---------------------------------------------------------
# Revisar estructura
# ---------------------------------------------------------

glimpse(timeseries_clean)

# ---------------------------------------------------------
# Construcción de panel mensual
# ---------------------------------------------------------

panel_desapariciones <- timeseries_clean %>%
  
  group_by(
    entidad,
    periodo
  ) %>%
  
  summarise(
    desapariciones = sum(total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  arrange(
    entidad,
    periodo
  )

# ---------------------------------------------------------
# Crear variable post tratamiento
# ---------------------------------------------------------

panel_desapariciones <- panel_desapariciones %>%
  
  mutate(
    
    post = if_else(
      periodo >= as.Date("2019-06-01"),
      1,
      0
    )
    
  )

# ---------------------------------------------------------
# Revisar panel
# ---------------------------------------------------------

glimpse(panel_desapariciones)

# ---------------------------------------------------------
# Revisar rango temporal
# ---------------------------------------------------------

panel_desapariciones %>%
  
  summarise(
    min_periodo = min(periodo),
    max_periodo = max(periodo)
  ) %>%
  
  print()

# ---------------------------------------------------------
# Revisar entidades
# ---------------------------------------------------------

panel_desapariciones %>%
  
  distinct(entidad) %>%
  
  arrange(entidad) %>%
  
  print(n = Inf)

# ---------------------------------------------------------
# Revisar balance del panel
# ---------------------------------------------------------

panel_balance <- panel_desapariciones %>%
  
  count(entidad)

print(panel_balance)

# ---------------------------------------------------------
# Guardar panel
# ---------------------------------------------------------

write_csv(
  panel_desapariciones,
  file.path(
    path_processed,
    "panel_desapariciones.csv"
  )
)

# ---------------------------------------------------------
# Mensaje final
# ---------------------------------------------------------

cat(
  "\n====================================\n",
  "Panel construido correctamente.\n",
  "====================================\n"
)