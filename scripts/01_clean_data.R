# =========================================================
# 01_clean_data.R
# Limpieza inicial de datos de desapariciones
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

path_raw <- "data/raw"
path_processed <- "data/processed"

# ---------------------------------------------------------
# Cargar datos
# ---------------------------------------------------------

data_raw <- read_csv(
  file.path(path_raw, "data.csv"),
  show_col_types = FALSE
)

timeseries_raw <- read_csv(
  file.path(path_raw, "timeseries_victimas.csv"),
  show_col_types = FALSE
)
# ---------------------------------------------------------
# Limpieza de nombres
# ---------------------------------------------------------

data_clean <- data_raw %>%
  clean_names()

timeseries_clean <- timeseries_raw %>%
  clean_names()

# ---------------------------------------------------------
# Revisar estructura
# ---------------------------------------------------------

glimpse(data_clean)

glimpse(timeseries_clean)

# ---------------------------------------------------------
# Convertir fechas
# ---------------------------------------------------------

# IMPORTANTE:
# Ajustar nombres de variables según estructura real

timeseries_clean <- timeseries_clean %>%
  mutate(
    periodo = as.Date(periodo)
  )
# ---------------------------------------------------------
# Revisar missing values
# ---------------------------------------------------------

missing_summary <- timeseries_clean %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  )

print(missing_summary)

# ---------------------------------------------------------
# Revisar rango temporal
# ---------------------------------------------------------

timeseries_clean %>%
  summarise(
    min_periodo = min(periodo, na.rm = TRUE),
    max_periodo = max(periodo, na.rm = TRUE)
  ) %>%
  print()

# ---------------------------------------------------------
# Revisar entidades
# ---------------------------------------------------------

timeseries_clean %>%
  distinct(entidad) %>%
  arrange(entidad) %>%
  print(n = Inf)

# ---------------------------------------------------------
# Guardar datasets limpios
# ---------------------------------------------------------

write_csv(
  data_clean,
  file.path(path_processed, "data_clean.csv")
)

write_csv(
  timeseries_clean,
  file.path(path_processed, "timeseries_clean.csv")
)

# ---------------------------------------------------------
# Fin del script
# ---------------------------------------------------------

cat(
  "\n====================================\n",
  "Limpieza completada correctamente.\n",
  "====================================\n"
)