# ============================================================
# Tarea 3 - Herramientas de Ciencia de Datos I
# Funciones para resolver los tres ejercicios
# ============================================================

# Funcion: alerta.movil
#
# Recibe:
#   df       : data frame con las transacciones y su fecha.
#   ventana  : numero de dias anteriores para el calculo.
#   k.alerta : multiplicador de la desviacion estandar.
#
# Devuelve:
#   Data frame con las columnas:
#   fecha, n.tx, media, desv y alerta.

alerta.movil <- function(df, ventana, k.alerta) {
  
  # Validar los parametros
  if (length(ventana) != 1 ||
      is.na(ventana) ||
      ventana < 2 ||
      ventana != as.integer(ventana)) {
    stop("ventana debe ser un entero mayor o igual a 2")
  }
  
  if (length(k.alerta) != 1 ||
      !is.finite(k.alerta)) {
    stop("k.alerta debe ser un numero finito")
  }
  
  # Contar las transacciones de cada fecha
  diario <- df |>
    dplyr::count(fecha, name = "n.tx") |>
    dplyr::arrange(fecha)
  
  # Convertir las fechas
  diario$fecha <- as.Date(diario$fecha)
  
  # Cantidad de dias
  n <- nrow(diario)
  
  if (ventana >= n) {
    stop("La ventana debe ser menor que los dias disponibles")
  }
  
  # Crear columnas de resultados
  diario$media <- NA_real_
  diario$desv <- NA_real_
  diario$alerta <- NA
  
  # Calcular estadisticas de los dias anteriores
  for (t in seq.int(ventana + 1L, n)) {
    
    anteriores <- diario$n.tx[
      (t - ventana):(t - 1L)
    ]
    
    diario$media[t] <- mean(anteriores)
    
    diario$desv[t] <- sd(anteriores)
    
    # Verificar si el dia genera alerta
    diario$alerta[t] <-
      diario$n.tx[t] >
      diario$media[t] + k.alerta * diario$desv[t]
  }
  
  return(as.data.frame(diario))
}

# ============================================================
# Tarea 3 - Herramientas de Ciencia de Datos I
# Funciones para resolver los tres ejercicios
# ============================================================

benford.mad <- function(x) {
  
  # Conservar únicamente montos positivos y válidos
  x <- x[is.finite(x) & x > 0]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  # Extraer el primer dígito significativo
  digito <- floor(x / 10^floor(log10(x)))
  
  # Frecuencias observadas de los dígitos 1 a 9
  frecuencias <- tabulate(digito, nbins = 9)
  p <- frecuencias / length(digito)
  
  # Probabilidades teóricas de Benford
  d <- 1:9
  b <- log10(1 + 1 / d)
  
  # Desviación absoluta media
  mad <- mean(abs(p - b))
  
  return(mad)
}

benford.canton <- function(df, canal, cantones) {
  
  library(dplyr)
  
  # Filtrar el canal solicitado
  datos.canal <- df |>
    filter(.data$canal == .env$canal)
  
  # Calcular MAD para cada cantón
  resultados <- datos.canal |>
    group_by(cod_canton) |>
    summarise(
      n = sum(is.finite(monto_crc) & monto_crc > 0),
      mad = benford.mad(monto_crc),
      .groups = "drop"
    )
  
  # Incorporar los nombres de los cantones
  resultados <- resultados |>
    left_join(cantones, by = "cod_canton")
  
  # Clasificar según Nigrini
  resultados <- resultados |>
    mutate(
      clase = case_when(
        is.na(mad) ~ NA_character_,
        mad <= 0.006 ~ "Conformidad cercana",
        mad <= 0.012 ~ "Conformidad aceptable",
        mad <= 0.015 ~ "Conformidad marginal",
        TRUE ~ "No conformidad"
      )
    ) |>
    select(cod_canton, canton, n, mad, clase) |>
    arrange(desc(mad))
  
  return(resultados)
}