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

