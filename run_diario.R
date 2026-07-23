# ============================================================
# Orquestador de la corrida diaria: clima.R -> pipeline_ndvi_powerbi.R
# ============================================================
# Pensado para llamarse desde el Programador de tareas de Windows via
# run_diario.bat (ver ese archivo para el horario configurado).
#
# Orden fijo: clima.R primero, pipeline_ndvi_powerbi.R despues. Si
# clima.R falla, NO se corre el pipeline (evita generar salidas con
# datos de clima desactualizados o con MAESTRO.csv sin refrescar).
#
# Cada corrida deja:
#  - logs/run_<timestamp>.log      : salida completa de ambos scripts
#  - logs/ultimo_estado.txt        : OK/FALLO + detalle de la ultima corrida
#
# El working directory se fija a la carpeta donde vive este script
# (asumiendo que esta al mismo nivel que R/ y data/), sin depender de
# que el Programador de tareas tenga bien puesto "Iniciar en".

args <- commandArgs(trailingOnly = FALSE)
script_arg <- args[grep("^--file=", args)]
if (length(script_arg) == 0) {
  repo_root <- getwd()  # corrida interactiva (RStudio, etc.)
} else {
  repo_root <- dirname(normalizePath(sub("^--file=", "", script_arg)))
}
setwd(repo_root)

dir.create("logs", showWarnings = FALSE)
log_file <- file.path("logs", paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", Sys.getpid(), ".log"))
estado_file <- file.path("logs", "ultimo_estado.txt")

con <- file(log_file, open = "wt")
sink(con, type = "output")
sink(con, type = "message")

log_linea <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", ..., "\n")

escribir_estado <- function(estado, detalle) {
  writeLines(
    c(paste("ESTADO:", estado),
      paste("FECHA:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste("DETALLE:", detalle),
      paste("LOG:", log_file)),
    estado_file
  )
}

correr_paso <- function(nombre, ruta_script) {
  log_linea("INICIO", nombre)
  resultado <- tryCatch({
    source(ruta_script, echo = FALSE)
    list(ok = TRUE, msg = "")
  }, error = function(e) {
    list(ok = FALSE, msg = conditionMessage(e))
  })
  log_linea(if (resultado$ok) "OK" else paste("FALLO -", resultado$msg), nombre)
  resultado
}

r_clima <- correr_paso("clima.R", "R/clima.R")

if (!r_clima$ok) {
  log_linea("Abortando: clima.R fallo, no se corre pipeline_ndvi_powerbi.R")
  sink(type = "message"); sink(type = "output"); close(con)
  escribir_estado("FALLO", paste("clima.R:", r_clima$msg))
  quit(status = 1, save = "no")
}

r_pipeline <- correr_paso("pipeline_ndvi_powerbi.R", "R/pipeline_ndvi_powerbi.R")

sink(type = "message"); sink(type = "output"); close(con)

if (r_pipeline$ok) {
  escribir_estado("OK", "clima.R y pipeline_ndvi_powerbi.R corrieron sin errores")
  quit(status = 0, save = "no")
} else {
  escribir_estado("FALLO", paste("pipeline_ndvi_powerbi.R:", r_pipeline$msg))
  quit(status = 1, save = "no")
}
