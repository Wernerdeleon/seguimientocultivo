# ==========================================================
# Normalizacion de codigos de finca/sector/lote (recodificacion SAP/Oracle)
# ==========================================================
# Contexto: se hizo una reestructuracion de Cecos que cambio EMPRESA y
# COD_FINCA para ~20 fincas (ver data/crosswalk_fincas.csv, construido a
# partir de "2._CeCos_Fincas_x.xlsx"). COD_SECTOR/COD_LOTE casi siempre se
# mantienen iguales, salvo 23 lotes que quedaron "Desactivado" (sin destino).
#
# Problema que resuelve: no se sabe con certeza si TODAS las tablas Oracle
# (sdeusr.lotes_imsa_gis en las dos instancias, agricola.m_lote,
# HISTORICOS.historico_lote, VW_INDICE_VEGETACION, VW_ANALISIS_SACAROSA) ya
# devuelven el codigo nuevo. Si una tabla quedo atras, los join por
# COD_FINCA fallan silenciosamente (ahi esta el problema de Occidente).
#
# Solucion: normalizar_finca() se aplica INMEDIATAMENTE despues de cada
# dbGetQuery(). Si la fila trae codigo viejo, la mapea al nuevo (canonico).
# Si ya trae el codigo nuevo, no encuentra match en el crosswalk y la deja
# igual (es un no-op seguro sin importar que tan migrada este cada tabla).

library(dplyr)
library(readr)

cargar_crosswalk_fincas <- function(ruta = "data/crosswalk_fincas.csv") {
  cw <- read_csv(ruta, show_col_types = FALSE)
  cw <- cw %>%
    mutate(
      FINCA_OLD  = as.double(FINCA_OLD),
      SECTOR_OLD = as.double(SECTOR_OLD),
      LOTE_OLD   = as.double(LOTE_OLD),
      FINCA_NEW  = as.double(FINCA_NEW),
      SECTOR_NEW = as.double(SECTOR_NEW),
      LOTE_NEW   = as.double(LOTE_NEW)
    ) %>%
    filter(!is.na(FINCA_NEW)) %>%              # excluye lotes "Desactivado" (sin destino): no se remapean
    distinct(FINCA_OLD, SECTOR_OLD, LOTE_OLD, .keep_all = TRUE)
  cw
}

# df debe tener COD_FINCA, COD_SECTOR, COD_LOTE (numericos). Si el df no
# trae COD_SECTOR/COD_LOTE (p.ej. la primera consulta de lotes_activos que
# solo trae c_finca), usar solo_finca = TRUE para mapear por FINCA sola.
normalizar_finca <- function(df, crosswalk, solo_finca = FALSE) {
  df <- df %>% mutate(COD_FINCA = as.double(COD_FINCA))

  if (solo_finca) {
    cw_finca <- crosswalk %>%
      distinct(FINCA_OLD, FINCA_NEW) %>%
      # si una FINCA_OLD tiene mas de un FINCA_NEW (no deberia) nos quedamos con el primero
      distinct(FINCA_OLD, .keep_all = TRUE)

    df <- df %>%
      left_join(cw_finca, by = c("COD_FINCA" = "FINCA_OLD")) %>%
      mutate(COD_FINCA = coalesce(FINCA_NEW, COD_FINCA)) %>%
      select(-FINCA_NEW)
    return(df)
  }

  stopifnot(all(c("COD_SECTOR","COD_LOTE") %in% names(df)))

  df <- df %>%
    mutate(COD_SECTOR = as.double(COD_SECTOR), COD_LOTE = as.double(COD_LOTE)) %>%
    left_join(crosswalk, by = c("COD_FINCA" = "FINCA_OLD", "COD_SECTOR" = "SECTOR_OLD", "COD_LOTE" = "LOTE_OLD")) %>%
    mutate(
      COD_FINCA  = coalesce(FINCA_NEW, COD_FINCA),
      COD_SECTOR = coalesce(SECTOR_NEW, COD_SECTOR),
      COD_LOTE   = coalesce(LOTE_NEW, COD_LOTE)
    ) %>%
    select(-FINCA_NEW, -SECTOR_NEW, -LOTE_NEW, -any_of("DESC_CECO"), -any_of("EMP_OLD"), -any_of("EMP_NEW"))

  df
}
