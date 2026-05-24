# =========================================================
# Control Sintético
# Dynamic Causal Inference - Guardia Nacional
# =========================================================

# ---------------------------------------------------------
# Librerías
# ---------------------------------------------------------

library(tidyverse)
library(lubridate)
library(Synth)

# ---------------------------------------------------------
# Configuración
# ---------------------------------------------------------

set.seed(1234)

options(scipen = 999)

# ---------------------------------------------------------
# Paths
# ---------------------------------------------------------

path_processed <- "data/processed"
path_images <- "docs/images"
path_tables <- "docs/tables"

dir.create(path_images, recursive = TRUE, showWarnings = FALSE)
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# Cargar panel
# ---------------------------------------------------------

panel_data <- read_csv(
  file.path(path_processed, "panel_desapariciones.csv"),
  show_col_types = FALSE
)

# ---------------------------------------------------------
# Preparar variables base
# ---------------------------------------------------------

panel_data <- panel_data %>%
  mutate(
    periodo = as.Date(periodo)
  ) %>%
  arrange(entidad, periodo) %>%
  mutate(
    entidad_id = as.numeric(factor(entidad))
  ) %>%
  group_by(periodo) %>%
  mutate(
    time_id = cur_group_id()
  ) %>%
  ungroup()

# ---------------------------------------------------------
# Revisar entidades incompletas
# ---------------------------------------------------------

balance_entidades <- panel_data %>%
  count(entidad, entidad_id) %>%
  arrange(n)

print(balance_entidades, n = Inf)

# ---------------------------------------------------------
# Remover entidades incompletas para Synth
# ---------------------------------------------------------
# Synth requiere panel perfectamente balanceado.
# En este caso, Oaxaca tiene menos periodos que el resto.

entidades_completas <- panel_data %>%
  count(entidad_id) %>%
  filter(n == max(n)) %>%
  pull(entidad_id)

panel_synth <- panel_data %>%
  filter(entidad_id %in% entidades_completas)

# ---------------------------------------------------------
# Verificar balance final
# ---------------------------------------------------------

panel_synth %>%
  count(entidad_id) %>%
  summarise(
    min_n = min(n),
    max_n = max(n)
  ) %>%
  print()

panel_synth %>%
  count(entidad_id, time_id) %>%
  filter(n > 1) %>%
  print()

# ---------------------------------------------------------
# Identificar Guanajuato
# ---------------------------------------------------------

guanajuato_id <- panel_synth %>%
  filter(entidad == "Guanajuato") %>%
  pull(entidad_id) %>%
  unique()

print(guanajuato_id)

panel_synth %>%
  filter(entidad == "Guanajuato") %>%
  summarise(n = n()) %>%
  print()

# ---------------------------------------------------------
# Definir periodos
# ---------------------------------------------------------

fecha_tratamiento <- ymd("2019-06-01")

pre_period <- panel_synth %>%
  filter(periodo < fecha_tratamiento) %>%
  pull(time_id) %>%
  unique() %>%
  sort()

all_periods <- panel_synth %>%
  pull(time_id) %>%
  unique() %>%
  sort()

# ---------------------------------------------------------
# Donor pool
# ---------------------------------------------------------

donor_ids <- setdiff(
  unique(panel_synth$entidad_id),
  guanajuato_id
)

# ---------------------------------------------------------
# Preparar datos para Synth
# ---------------------------------------------------------

dataprep_out <- dataprep(
  foo = as.data.frame(panel_synth),
  predictors = c("desapariciones"),
  predictors.op = "mean",
  dependent = "desapariciones",
  unit.variable = "entidad_id",
  unit.names.variable = "entidad",
  time.variable = "time_id",
  treatment.identifier = guanajuato_id,
  controls.identifier = donor_ids,
  time.predictors.prior = pre_period,
  time.optimize.ssr = pre_period,
  time.plot = all_periods
)

# ---------------------------------------------------------
# Estimar control sintético
# ---------------------------------------------------------

synth_out <- synth(dataprep_out)

# ---------------------------------------------------------
# Tablas de pesos
# ---------------------------------------------------------

synth_tables <- synth.tab(
  dataprep.res = dataprep_out,
  synth.res = synth_out
)

pesos_synth <- synth_tables$tab.w

print(pesos_synth)

write_csv(
  as_tibble(pesos_synth),
  file.path(path_tables, "pesos_synth_guanajuato.csv")
)

# ---------------------------------------------------------
# Gráfica: Guanajuato vs Guanajuato sintético
# ---------------------------------------------------------

png(
  filename = file.path(path_images, "synth_control_guanajuato.png"),
  width = 1200,
  height = 800
)

path.plot(
  synth.res = synth_out,
  dataprep.res = dataprep_out,
  Ylab = "Desapariciones",
  Xlab = "Periodo",
  Legend = c("Guanajuato", "Guanajuato sintético"),
  Main = "Control Sintético: Guanajuato"
)

abline(v = length(pre_period), lty = 2)

dev.off()

# ---------------------------------------------------------
# Gráfica: brecha observada - sintética
# ---------------------------------------------------------

png(
  filename = file.path(path_images, "synth_gap_guanajuato.png"),
  width = 1200,
  height = 800
)

gaps.plot(
  synth.res = synth_out,
  dataprep.res = dataprep_out,
  Ylab = "Brecha",
  Xlab = "Periodo",
  Main = "Brecha entre Guanajuato observado y sintético"
)

abline(v = length(pre_period), lty = 2)

dev.off()

# ---------------------------------------------------------
# Guardar objetos
# ---------------------------------------------------------

saveRDS(
  synth_out,
  file.path(path_tables, "synth_out_guanajuato.rds")
)

saveRDS(
  dataprep_out,
  file.path(path_tables, "dataprep_out_guanajuato.rds")
)

# ---------------------------------------------------------
# Fin
# ---------------------------------------------------------

cat(
  "\n====================================\n",
  "Control sintético completado correctamente.\n",
  "====================================\n"
)