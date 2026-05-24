# =========================================================
# EVENT STUDY - Guardia Nacional
# =========================================================

# ---------------------------------------------------------
# Librerías
# ---------------------------------------------------------

library(tidyverse)
library(lubridate)
library(fixest)

# ---------------------------------------------------------
# Cargar panel
# ---------------------------------------------------------

panel_data <- read_csv(
  "data/processed/panel_desapariciones.csv"
)

# ---------------------------------------------------------
# Fecha de tratamiento
# ---------------------------------------------------------

fecha_tratamiento <- ymd("2019-06-01")

# ---------------------------------------------------------
# Crear tiempo relativo al tratamiento
# ---------------------------------------------------------

panel_data <- panel_data %>%
  mutate(
    periodo = as.Date(periodo),
    treated = if_else(entidad == "Guanajuato", 1, 0),
    event_time = interval(
      ymd("2019-06-01"),
      periodo
    ) %/% months(1)
  )

# Revisar
summary(panel_data$event_time)

# ---------------------------------------------------------
# Modelo Event Study
# ---------------------------------------------------------

modelo_event <- feols(
  desapariciones ~
    i(event_time, treated, ref = -1) |
    entidad + periodo,
  data = panel_data
)

summary(modelo_event)


# ---------------------------------------------------------
# Gráfica Event Study
# ---------------------------------------------------------

grafica_event <- iplot(
  modelo_event,
  main = "Event Study: Guanajuato",
  xlab = "Mes relativo al tratamiento",
  ylab = "Efecto estimado"
)

# ---------------------------------------------------------
# Guardar gráfica
# ---------------------------------------------------------

png(
  filename = "docs/images/event_study.png",
  width = 1200,
  height = 800
)

iplot(
  modelo_event,
  main = "Event Study: Guanajuato",
  xlab = "Mes relativo al tratamiento",
  ylab = "Efecto estimado"
)

dev.off()
