# =========================================================
# 07_causal_impact.R
# CausalImpact (BSTS) - Efecto de la Guardia Nacional
# sobre víctimas de delitos en Guanajuato
#
# Fuente: data/processed/panel_desapariciones.csv
# Generado por 01_clean_data.R + 02_build_panel.R
# (timeseries_victimas.csv, todos los delitos SESNSP)
#
# Método: Brodersen et al. (2015), Ann. Appl. Stat. 9(1)
# DOI: 10.1214/14-AOAS788
# =========================================================

# ---------------------------------------------------------
# Librerías
# ---------------------------------------------------------

library(tidyverse)
library(lubridate)
library(zoo)
library(CausalImpact)  

# ---------------------------------------------------------
# Configuración
# ---------------------------------------------------------

set.seed(1234)
options(scipen = 999)

# ---------------------------------------------------------
# Paths
# ---------------------------------------------------------

path_processed <- "data/processed"
path_images    <- "docs/images"
path_tables    <- "docs/tables"

dir.create(path_images, recursive = TRUE, showWarnings = FALSE)
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# Cargar panel (generado por 01 + 02)
# ---------------------------------------------------------

panel_data <- read_csv(
  "/Users/manueldelatejera/Desktop/Maestria/2do_Semestre/Metodos_Analíticos/EntregaFinal/dynamic-causal-inference/data/processed/panel_desapariciones.csv"
) %>%
  mutate(periodo = as.Date(periodo)) %>%
  arrange(entidad, periodo)

glimpse(panel_data)

# ---------------------------------------------------------
# Verificar que Guanajuato está presente
# El nombre viene de clean_names() sobre timeseries_victimas,
# que conserva el nombre original: "Guanajuato"
# ---------------------------------------------------------

entidades_disponibles <- unique(panel_data$entidad)

if (!"Guanajuato" %in% entidades_disponibles) {
  cat("\nEntidades disponibles en el panel:\n")
  print(sort(entidades_disponibles))
  stop(paste(
    "Guanajuato no encontrado. Verifica el nombre exacto arriba",
    "y actualiza la variable 'nombre_guanajuato' abajo."
  ))
}

nombre_guanajuato  <- "Guanajuato"
nombre_edo_mexico  <- "México"   # nombre en timeseries_victimas

cat("\nGuanajuato encontrado correctamente.\n")

# ---------------------------------------------------------
# Remover entidades incompletas (igual que 06_synth_control)
# Oaxaca tiene menos periodos que el resto
# ---------------------------------------------------------

entidades_completas <- panel_data %>%
  count(entidad) %>%
  filter(n == max(n)) %>%
  pull(entidad)

panel_data <- panel_data %>%
  filter(entidad %in% entidades_completas)

cat("Entidades con panel completo:", length(entidades_completas), "\n")
cat(paste(sort(entidades_completas), collapse = ", "), "\n\n")

# ---------------------------------------------------------
# Construir formato wide para CausalImpact
# ---------------------------------------------------------

panel_wide <- panel_data %>%
  select(entidad, periodo, desapariciones) %>%
  pivot_wider(
    names_from  = entidad,
    values_from = desapariciones
  ) %>%
  arrange(periodo)

fechas <- panel_wide$periodo

# ---------------------------------------------------------
# Separar serie objetivo y covariables
#
# Se excluye Estado de México ("México" en timeseries)
# por ser unidad atípica en niveles (ver EDA, Sección 2.6).
# Esta decisión es consistente con el tratamiento en
# 06_synth_control.R, donde el algoritmo le asignó peso 0.
# ---------------------------------------------------------

y_guanajuato <- panel_wide %>% pull(all_of(nombre_guanajuato))

covariables <- panel_wide %>%
  select(
    -periodo,
    -all_of(nombre_guanajuato),
    -all_of(nombre_edo_mexico)
  )

cat("Serie objetivo: Guanajuato\n")
cat("Covariables:", ncol(covariables), "entidades\n")
cat(paste(names(covariables), collapse = ", "), "\n\n")

# ---------------------------------------------------------
# Objeto zoo
# ---------------------------------------------------------

datos_zoo <- zoo(
  cbind(y_guanajuato, covariables),
  order.by = fechas
)

# ---------------------------------------------------------
# Ventanas pre y post tratamiento
# ---------------------------------------------------------

fecha_intervencion <- as.Date("2019-06-01")

pre_period  <- c(min(fechas), fecha_intervencion %m-% months(1))
post_period <- c(fecha_intervencion, max(fechas))

cat("Ventana pre-tratamiento: ",
    format(pre_period[1],  "%Y-%m"), "a",
    format(pre_period[2],  "%Y-%m"), "\n")
cat("Ventana post-tratamiento:",
    format(post_period[1], "%Y-%m"), "a",
    format(post_period[2], "%Y-%m"), "\n")
cat("Periodos pre: ",
    interval(pre_period[1],  pre_period[2])  %/% months(1) + 1, "\n")
cat("Periodos post:",
    interval(post_period[1], post_period[2]) %/% months(1) + 1, "\n\n")

# ---------------------------------------------------------
# Estimar CausalImpact (BSTS)
#
# niter = 5000: iteraciones MCMC para convergencia estable.
# standardize.data = TRUE: estandariza covariables antes
#   del spike-and-slab, evitando que entidades con mayor
#   volumen dominen la selección de variables.
# ---------------------------------------------------------

cat("Estimando modelo BSTS... (puede tardar ~1-2 minutos)\n")

impact <- CausalImpact(
  data        = datos_zoo,
  pre.period  = pre_period,
  post.period = post_period,
  model.args  = list(
    niter            = 5000,
    standardize.data = TRUE
  )
)

# ---------------------------------------------------------
# Resumen estadístico
# ---------------------------------------------------------

cat("\n========= RESUMEN CAUSALIMPACT =========\n")
summary(impact)

cat("\n========= INTERPRETACIÓN EN TEXTO =========\n")
summary(impact, "report")

# ---------------------------------------------------------
# Extraer métricas para el reporte
# ---------------------------------------------------------

resumen <- impact$summary

ef_abs_media  <- resumen["Average", "AbsEffect"]
ef_abs_lower  <- resumen["Average", "AbsEffect.lower"]
ef_abs_upper  <- resumen["Average", "AbsEffect.upper"]
ef_rel_media  <- resumen["Average", "RelEffect"] * 100
ef_rel_lower  <- resumen["Average", "RelEffect.lower"] * 100
ef_rel_upper  <- resumen["Average", "RelEffect.upper"] * 100
p_valor       <- resumen["Average", "p"]

cat(sprintf(
  "\nEfecto absoluto promedio: %.1f (IC 95%%: [%.1f, %.1f])\n",
  ef_abs_media, ef_abs_lower, ef_abs_upper
))
cat(sprintf(
  "Efecto relativo promedio: %.1f%% (IC 95%%: [%.1f%%, %.1f%%])\n",
  ef_rel_media, ef_rel_lower, ef_rel_upper
))
cat(sprintf("p-valor bayesiano: %.4f\n\n", p_valor))

# ---------------------------------------------------------
# Gráfica 1: Serie observada vs contrafactual BSTS
# ---------------------------------------------------------

series_ci <- impact$series %>%
  as.data.frame() %>%
  rownames_to_column("periodo") %>%
  mutate(periodo = as.Date(periodo))

grafica_contrafactual <- ggplot(series_ci, aes(x = periodo)) +
  geom_ribbon(
    aes(ymin = point.pred.lower, ymax = point.pred.upper),
    fill  = "#4472C4",
    alpha = 0.20
  ) +
  geom_line(
    aes(y = point.pred, color = "Contrafactual BSTS"),
    linewidth = 1,
    linetype  = "dashed"
  ) +
  geom_line(
    aes(y = response, color = "Guanajuato observado"),
    linewidth = 1
  ) +
  geom_vline(
    xintercept = fecha_intervencion,
    linetype   = "dotted",
    linewidth  = 0.8,
    color      = "black"
  ) +
  annotate(
    "text",
    x     = fecha_intervencion + days(20),
    y     = max(series_ci$response, na.rm = TRUE) * 0.93,
    label = "Guardia Nacional\n(jun 2019)",
    hjust = 0,
    size  = 3.2
  ) +
  scale_color_manual(
    values = c(
      "Guanajuato observado" = "#C0392B",
      "Contrafactual BSTS"   = "#4472C4"
    )
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Guanajuato: serie observada vs contrafactual BSTS",
    subtitle = "Banda sombreada: intervalo de credibilidad al 95%",
    x        = "Periodo",
    y        = "Víctimas de delitos (SESNSP)",
    color    = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  filename = "causal_impact_contrafactual.png",
  plot     = grafica_contrafactual,
  path     = path_images,
  width    = 10,
  height   = 5.5,
  dpi      = 150
)

# ---------------------------------------------------------
# Gráfica 2: Efecto causal puntual (brecha)
# ---------------------------------------------------------

grafica_brecha <- ggplot(series_ci, aes(x = periodo)) +
  geom_ribbon(
    aes(ymin = point.effect.lower, ymax = point.effect.upper),
    fill  = "#27AE60",
    alpha = 0.25
  ) +
  geom_line(
    aes(y = point.effect),
    color     = "#1E8449",
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dashed",
    linewidth  = 0.7
  ) +
  geom_vline(
    xintercept = fecha_intervencion,
    linetype   = "dotted",
    linewidth  = 0.8,
    color      = "black"
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Efecto causal puntual estimado por CausalImpact",
    subtitle = "Banda sombreada: intervalo de credibilidad al 95%",
    x        = "Periodo",
    y        = "Efecto estimado (víctimas)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = "causal_impact_brecha.png",
  plot     = grafica_brecha,
  path     = path_images,
  width    = 10,
  height   = 4.5,
  dpi      = 150
)

cat("Gráficas guardadas en", path_images, "\n")

# ---------------------------------------------------------
# Guardar objeto impact (el Reporte.Rmd lo carga con readRDS)
# ---------------------------------------------------------

saveRDS(
  impact,
  file.path(path_tables, "causal_impact_guanajuato.rds")
)

# ---------------------------------------------------------
# Tabla resumen
# ---------------------------------------------------------

tabla_resumen <- tibble(
  Metrica = c(
    "Predicción contrafactual promedio",
    "Valor observado promedio",
    "Efecto absoluto promedio",
    "IC 95% inferior",
    "IC 95% superior",
    "Efecto relativo promedio (%)",
    "p-valor bayesiano"
  ),
  Valor = c(
    round(resumen["Average", "Pred"],     2),
    round(resumen["Average", "Actual"],   2),
    round(ef_abs_media,                   2),
    round(ef_abs_lower,                   2),
    round(ef_abs_upper,                   2),
    round(ef_rel_media,                   2),
    round(p_valor,                        4)
  )
)

write_csv(
  tabla_resumen,
  file.path(path_tables, "tabla_causal_impact.csv")
)

print(tabla_resumen)

# ---------------------------------------------------------
# Fin del script
# ---------------------------------------------------------

cat(
  "\n====================================\n",
  "CausalImpact completado correctamente.\n",
  "====================================\n"
)
