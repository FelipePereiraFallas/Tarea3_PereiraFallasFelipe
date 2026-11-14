# ============================================================
# Tarea 3 - Herramientas de Ciencia de Datos I
# Funciones para resolver los tres ejercicios
# ============================================================

valor.acumulado <- function(df, tasa.anual, canal) {
  
  library(dplyr)
  
  # Verificar que la tasa sea válida
  if (!is.numeric(tasa.anual) ||
      length(tasa.anual) != 1 ||
      is.na(tasa.anual) ||
      tasa.anual <= -1) {
    stop("La tasa anual debe ser un número mayor que -1.")
  }
  
  # Convertir fecha y filtrar por canal
  datos <- df |>
    mutate(fecha = as.Date(fecha)) |>
    filter(.data$canal == .env$canal)
  
  # Agrupar los montos por mes
  tabla <- datos |>
    mutate(mes = as.integer(format(fecha, "%m"))) |>
    group_by(mes) |>
    summarise(
      monto = sum(monto_crc, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Incluir los 12 meses, incluso si alguno no tiene transacciones
  tabla <- data.frame(mes = 1:12) |>
    left_join(tabla, by = "mes") |>
    mutate(monto = coalesce(monto, 0))
  
  # Calcular la tasa mensual equivalente
  tasa.mensual <- (1 + tasa.anual)^(1 / 12) - 1
  
  # Calcular el factor de acumulación
  tabla <- tabla |>
    mutate(
      factor = (1 + tasa.mensual)^(12 - mes)
    )
  
  # Calcular el valor acumulado
  va <- sum(tabla$monto * tabla$factor)
  
  # Calcular los intereses
  interes <- va - sum(tabla$monto)
  
  return(
    list(
      tabla = tabla,
      tasa.mensual = tasa.mensual,
      va = va,
      interes = interes
    )
  )
}