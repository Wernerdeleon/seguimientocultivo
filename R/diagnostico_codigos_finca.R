# ==========================================================
# Diagnostico rapido (solo lectura, sin loops pesados, no escribe nada)
# ==========================================================
# Objetivo: antes de correr el pipeline completo, ver que codigo de finca
# (viejo o nuevo) trae CADA fuente Oracle hoy, y confirmar que despues de
# normalizar_finca() todas quedan alineadas bajo el codigo nuevo.
#
# Correr con el working directory en la raiz del repo (donde estan las
# carpetas R/ y data/).

library(RJDBC)
library(dplyr)
source("R/normalizar_fincas.R")
crosswalk_fincas <- cargar_crosswalk_fincas("data/crosswalk_fincas.csv")

driver <- RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver","C:/driver/ojdbc7.jar")

# --- Instancia IMSA (10.40.1.189) ---
con1 <- dbConnect(driver, "jdbc:oracle:thin:@10.40.1.189:1521/IMSA","hpaiz","Agosto2026")
gis1      <- dbGetQuery(con1, "select distinct c_finca as COD_FINCA from sdeusr.lotes_imsa_gis")
mlote     <- dbGetQuery(con1, "select distinct cod_finca as COD_FINCA from agricola.m_lote")
historico <- dbGetQuery(con1, "select distinct cod_finca as COD_FINCA from HISTORICOS.historico_lote")
dbDisconnect(con1)

# --- Instancia IMSAPST ---
con2 <- dbConnect(driver, "jdbc:oracle:thin:@IMSAPST:1521/IMSAPSTIA","USR_INVES","sfDezcRHhC")
gis2     <- dbGetQuery(con2, "select distinct c_finca as COD_FINCA from sdeusr.lotes_imsa_gis")
indices  <- dbGetQuery(con2, "select distinct cod_finca as COD_FINCA from SDEUSR.VW_INDICE_VEGETACION where fecha_imagen >= TO_DATE('2025-11-01','YYYY-MM-DD')")
sacarosa <- dbGetQuery(con2, "select distinct cod_finca as COD_FINCA from SDEUSR.VW_ANALISIS_SACAROSA where ANO_ZAFRA = '2025/2026'")
dbDisconnect(con2)

fuentes <- list(
  "GIS (IMSA)"           = gis1,
  "m_lote"               = mlote,
  "historico_lote"       = historico,
  "GIS (IMSAPST)"        = gis2,
  "VW_INDICE_VEGETACION" = indices,
  "VW_ANALISIS_SACAROSA" = sacarosa
)

cat("=== Codigos de finca CRUDOS (antes de normalizar), por fuente ===\n")
for (nombre in names(fuentes)) {
  codigos <- sort(unique(as.double(fuentes[[nombre]]$COD_FINCA)))
  cat(sprintf("%-22s: %s%s\n", nombre,
              paste(head(codigos, 15), collapse = ", "),
              if (length(codigos) > 15) " ..." else ""))
}

# Fincas Occidente/Magdalena esperadas en codigo NUEVO tras normalizar
fincas_occidente_nuevo <- c(5001,5002,5003,5004,5005,5006,5008,5010,5011,
                            5012,5013,5014,5015,5016,5017,5018,5019,5020,
                            5030,5031)

cat("\n=== Fincas Occidente/Magdalena presentes DESPUES de normalizar_finca() ===\n")
for (nombre in names(fuentes)) {
  norm <- normalizar_finca(fuentes[[nombre]], crosswalk_fincas, solo_finca = TRUE)
  presentes <- intersect(fincas_occidente_nuevo, unique(norm$COD_FINCA))
  cat(sprintf("%-22s: %d de %d  (%s)\n", nombre,
              length(presentes), length(fincas_occidente_nuevo),
              paste(sort(presentes), collapse = ",")))
}

cat("\nSi despues de normalizar todas las fuentes muestran un numero similar\n",
    "de fincas Occidente presentes, el join deberia funcionar en el pipeline\n",
    "completo. Si alguna fuente sigue en 0, esa tabla probablemente todavia\n",
    "usa el codigo viejo bajo un patron distinto al del crosswalk (avisame).\n")
