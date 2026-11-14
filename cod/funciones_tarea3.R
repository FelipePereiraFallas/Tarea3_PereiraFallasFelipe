# ============================================================
# Tarea 3 - Herramientas de Ciencia de Datos I
# Funciones para resolver los tres ejercicios
# ============================================================

# ------------------------------------------------------------
# Función: benford.mad
# Recibe: x, vector numérico de montos.
# Devuelve: MAD respecto a la distribución de Benford.
# ------------------------------------------------------------

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