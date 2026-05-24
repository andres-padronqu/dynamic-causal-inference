# =========================================================
# Proyecto Final - Métodos Analíticos
# Difference-in-Differences (DID)
# =========================================================

# ---------------------------------------------------------
# Librerías
# ---------------------------------------------------------

library(tidyverse)
library(lubridate)
library(fixest)

# ---------------------------------------------------------
# Paths
# ---------------------------------------------------------

path_processed <- "data/processed"
path_tables <- "docs/tables"

dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# Cargar panel
# ---------------------------------------------------------

panel_data <- read_csv(
  file.path(path_processed, "panel_desapariciones.csv"),
  show_col_types = FALSE
)

# ---------------------------------------------------------
# Revisar estructura
# ---------------------------------------------------------

glimpse(panel_data)

# ---------------------------------------------------------
# Convertir periodo
# ---------------------------------------------------------

panel_data <- panel_data %>%
  mutate(
    periodo = as.Date(periodo)
  )

# ---------------------------------------------------------
# Construcción de variables DID
# ---------------------------------------------------------

# Tratamiento:
# Guanajuato = 1
# Otros estados = 0

panel_data <- panel_data %>%
  mutate(
    treated = if_else(
      entidad == "Guanajuato",
      1,
      0
    )
  )

# ---------------------------------------------------------
# Definir periodo post-tratamiento
# ---------------------------------------------------------

# Junio 2019:
# implementación de la Guardia Nacional

fecha_tratamiento <- as.Date("2019-06-01")

panel_data <- panel_data %>%
  mutate(
    post = if_else(
      periodo >= fecha_tratamiento,
      1,
      0
    )
  )

# ---------------------------------------------------------
# Variable DID
# ---------------------------------------------------------

panel_data <- panel_data %>%
  mutate(
    did = treated * post
  )

# ---------------------------------------------------------
# Revisar variables construidas
# ---------------------------------------------------------

panel_data %>%
  count(treated)

panel_data %>%
  count(post)

panel_data %>%
  count(did)

# ---------------------------------------------------------
# Construir serie agregada para pre-trends
# ---------------------------------------------------------

pretrend_data <- panel_data %>%
  group_by(periodo, treated) %>%
  summarise(
    desapariciones = sum(desapariciones, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    grupo = if_else(
      treated == 1,
      "Guanajuato",
      "Control"
    )
  )

# ---------------------------------------------------------
# Gráfica de tendencias paralelas
# ---------------------------------------------------------

grafica_pretrends <- ggplot(
  pretrend_data,
  aes(
    x = periodo,
    y = desapariciones,
    color = grupo
  )
) +
  geom_line(size = 1.1) +
  geom_vline(
    xintercept = fecha_tratamiento,
    linetype = "dashed"
  ) +
  labs(
    title = "Pre-trends: Guanajuato vs grupo de control",
    subtitle = "Línea punteada: implementación de la Guardia Nacional",
    x = "Periodo",
    y = "Número de desapariciones",
    color = "Grupo"
  ) +
  theme_minimal()

print(grafica_pretrends)


# ---------------------------------------------------------
# Guardar gráfica
# ---------------------------------------------------------

ggsave(
  filename = "pretrends_did.png",
  plot = grafica_pretrends,
  path = "docs/images",
  width = 10,
  height = 6
)

# ---------------------------------------------------------
# Modelo DID básico
# ---------------------------------------------------------

modelo_did <- feols(
  desapariciones ~ treated * post,
  data = panel_data
)

summary(modelo_did)
# ---------------------------------------------------------
# Modelo DID con efectos fijos
# ---------------------------------------------------------

modelo_did_fe <- feols(
  desapariciones ~ treated * post |
    entidad + periodo,
  data = panel_data
)

summary(modelo_did_fe)

# ---------------------------------------------------------
# Guardar resultados
# ---------------------------------------------------------

etable(
  modelo_did,
  modelo_did_fe,
  file = file.path(path_tables, "tabla_did.tex")
)

# ---------------------------------------------------------
# Fin del script
# ---------------------------------------------------------