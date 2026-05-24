# =========================================================
# 03_eda.R
# Exploratory Data Analysis
# Proyecto:
# Dynamic Causal Inference - Guardia Nacional
# =========================================================

# ---------------------------------------------------------
# Librerías
# ---------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(scales)

# ---------------------------------------------------------
# Configuración
# ---------------------------------------------------------

set.seed(1234)

theme_set(theme_bw())

options(scipen = 999)

# ---------------------------------------------------------
# Paths
# ---------------------------------------------------------

path_processed <- "data/processed"

# ---------------------------------------------------------
# Cargar panel
# ---------------------------------------------------------

panel_desapariciones <- read_csv(
  file.path(
    path_processed,
    "panel_desapariciones.csv"
  ),
  show_col_types = FALSE
)

# ---------------------------------------------------------
# Revisar estructura
# ---------------------------------------------------------

glimpse(panel_desapariciones)

# =========================================================
# 1. Serie temporal nacional
# =========================================================

serie_nacional <- panel_desapariciones %>%
  
  group_by(periodo) %>%
  
  summarise(
    desapariciones = sum(
      desapariciones,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

grafica_nacional <- ggplot(
  serie_nacional,
  aes(
    x = periodo,
    y = desapariciones
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_vline(
    xintercept = as.Date("2019-06-01"),
    linetype = "dashed",
    color = "red"
  ) +
  
  scale_y_continuous(
    labels = comma
  ) +
  
  labs(
    title = "Desapariciones en México a través del tiempo",
    subtitle = "Línea roja: implementación de la Guardia Nacional",
    x = "Periodo",
    y = "Número de desapariciones"
  )

print(grafica_nacional)

# ---------------------------------------------------------
# Guardar gráfica
# ---------------------------------------------------------

ggsave(
  filename = "docs/images/grafica_nacional.png",
  plot = grafica_nacional,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================================
# 2. Estados con más desapariciones
# =========================================================

top_estados <- panel_desapariciones %>%
  
  group_by(entidad) %>%
  
  summarise(
    total_desapariciones = sum(
      desapariciones,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  arrange(
    desc(total_desapariciones)
  )

print(top_estados)

# ---------------------------------------------------------
# Top 10
# ---------------------------------------------------------

top_10 <- top_estados %>%
  
  slice_max(
    total_desapariciones,
    n = 10
  )

grafica_top_10 <- ggplot(
  top_10,
  aes(
    x = reorder(
      entidad,
      total_desapariciones
    ),
    y = total_desapariciones
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  scale_y_continuous(
    labels = comma
  ) +
  
  labs(
    title = "Top 10 entidades con más desapariciones",
    x = "",
    y = "Total de desapariciones"
  )

print(grafica_top_10)

# ---------------------------------------------------------
# Guardar gráfica
# ---------------------------------------------------------

ggsave(
  filename = "docs/images/top_10_estados.png",
  plot = grafica_top_10,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================================
# 3. Tendencias temporales por estado
# =========================================================

top_5_estados <- top_10 %>%
  slice(1:5) %>%
  pull(entidad)

panel_top_5 <- panel_desapariciones %>%
  
  filter(
    entidad %in% top_5_estados
  )

grafica_top_5 <- ggplot(
  panel_top_5,
  aes(
    x = periodo,
    y = desapariciones,
    color = entidad
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_vline(
    xintercept = as.Date("2019-06-01"),
    linetype = "dashed"
  ) +
  
  scale_y_continuous(
    labels = comma
  ) +
  
  labs(
    title = "Tendencias temporales de desapariciones",
    subtitle = "Top 5 entidades",
    x = "Periodo",
    y = "Desapariciones"
  )

print(grafica_top_5)

# ---------------------------------------------------------
# Guardar gráfica
# ---------------------------------------------------------

ggsave(
  filename = "docs/images/tendencias_top_5.png",
  plot = grafica_top_5,
  width = 12,
  height = 7,
  dpi = 300
)

# =========================================================
# Fin del script
# =========================================================

cat(
  "\n====================================\n",
  "EDA completado correctamente.\n",
  "====================================\n"
)