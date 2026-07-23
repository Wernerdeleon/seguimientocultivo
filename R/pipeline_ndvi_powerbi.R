if(require(RJDBC)==FALSE){install.packages("RJDBC",dependencies = TRUE)}
library(dplyr)

# --- NORMALIZACION CODIGOS FINCA (fix datos Occidente) ---------------------
# Recodificacion SAP/Cecos: ~20 fincas cambiaron EMPRESA/COD_FINCA (ver
# data/crosswalk_fincas.csv y R/normalizar_fincas.R). No sabemos con certeza
# si TODAS las tablas Oracle ya devuelven el codigo nuevo, asi que se
# normaliza a codigo NUEVO (canonico) inmediatamente despues de cada
# dbGetQuery. Si una tabla ya trae el codigo nuevo, la funcion no encuentra
# match en el crosswalk y no cambia nada (no-op seguro).
source("R/normalizar_fincas.R")
crosswalk_fincas <- cargar_crosswalk_fincas("data/crosswalk_fincas.csv")
# -----------------------------------------------------------------------

driver <- RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver","C:/driver/ojdbc7.jar")
conexion <- dbConnect(driver, "jdbc:oracle:thin:@IMSAPST:1521/IMSAPSTIA","USR_INVES","sfDezcRHhC")
lotes_activos <- dbGetQuery(conexion,"select c_finca from sdeusr.lotes_imsa_gis")
colnames(lotes_activos) <- c("COD_FINCA")
lotes_activos$COD_FINCA <- as.double(lotes_activos$COD_FINCA)
lotes_activos <- normalizar_finca(lotes_activos, crosswalk_fincas, solo_finca = TRUE)   # <- fix
lotes_activos <- distinct(lotes_activos)

mlote <- dbGetQuery(conexion,"select COD_FINCA, COD_SECTOR, COD_LOTE, FECHA_ULTIMO_CORTE, FECHA_FINALIZO_CORTE, FECHA_SIEMBRA, ACTIVO, LOTE_SEMILLERO, AREA, AREA_CULTIVO, AREA_CORTADA, COD_VARIEDAD from agricola.m_lote")
mlote <- normalizar_finca(mlote, crosswalk_fincas)                                       # <- fix
mlote <- left_join(lotes_activos, mlote, by = c("COD_FINCA"))
mlote_historico <- dbGetQuery(conexion,"select ANO_ZAFRA, COD_FINCA, COD_SECTOR, COD_LOTE, FECHA_SIEMBRA, FECHA_ULTIMO_CORTE, FECHA_FINALIZO_CORTE, LOTE_SEMILLERO, AREA, AREA_CULTIVO, AREA_CORTADA, COD_VARIEDAD from HISTORICOS.historico_lote")
mlote_historico <- normalizar_finca(mlote_historico, crosswalk_fincas)                   # <- fix
mlote_historico <- left_join(lotes_activos, mlote_historico, by = c("COD_FINCA"))

m_lote <- mlote %>%
  subset(!COD_SECTOR == 63)%>%
  select(COD_FINCA, COD_SECTOR, COD_LOTE, FECHA_ULTIMO_CORTE, FECHA_FINALIZO_CORTE, FECHA_SIEMBRA, ACTIVO, LOTE_SEMILLERO, AREA, AREA_CULTIVO, AREA_CORTADA, COD_VARIEDAD)

historico <- mlote_historico %>%
  select(ANO_ZAFRA, COD_FINCA, COD_SECTOR, COD_LOTE, FECHA_SIEMBRA, FECHA_ULTIMO_CORTE, FECHA_FINALIZO_CORTE, LOTE_SEMILLERO, AREA, AREA_CULTIVO, AREA_CORTADA, COD_VARIEDAD)

historico <- subset(historico, !ANO_ZAFRA %in% c(sort(unique(historico$ANO_ZAFRA))[1:24]))

# ==========================================
# Cálculo y validación de temporadas por lote
# ==========================================

library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(writexl)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(readr)

# ====================================================================
# PARÁMETROS DE ZAFRA (CAMBIAR UNA VEZ AL AÑO)
# --------------------------------------------------------------------
# El inicio real de la zafra lo decide el ingenio y varía cada año.
# Solo estas dos fechas se editan a mano; el resto (dias_izafra,
# dias_izafra2 y el cutoff por mes de cosecha) se deriva de aquí.

INICIO_ZAFRA_ACTUAL   <- as.Date("2025-11-07")  # zafra 2026/2027
INICIO_ZAFRA_ANTERIOR <- as.Date("2024-11-03")  # zafra 2025/2026

# Offset de meses por mes-etiqueta de cosecha. Esto NO cambia año a año:
# es cuánto antes arranca cada cohorte respecto a noviembre.
OFFSET_MES_COSECHA <- c(
  noviembre = 0, diciembre = 1, enero = 2,
  febrero   = 3, marzo     = 4, abril = 5, mayo = 6
)

# Meses transcurridos desde el inicio de cada zafra hasta hoy.
# Reemplaza los cutoffs manuales (los 13/13/... y 9,8,7,6,5,4 hardcodeados).
# Se usa ceiling (redondeo hacia arriba) para replicar el criterio manual:
# el mes en curso se cuenta como ya empezado (da 9, no 8).
meses_zafra_actual   <- as.integer(ceiling(interval(INICIO_ZAFRA_ACTUAL,   Sys.Date()) / months(1)))
meses_zafra_anterior <- as.integer(ceiling(interval(INICIO_ZAFRA_ANTERIOR, Sys.Date()) / months(1)))

# Días transcurridos desde el inicio de cada zafra (usados en los filtros de dpuntos).
# Antes estaban hardcodeados como Sys.Date() - "2024-11-03" / "2025-11-07".
dias_izafra  <- as.numeric(Sys.Date() - INICIO_ZAFRA_ANTERIOR)   # zafra 2025/2026
dias_izafra2 <- as.numeric(Sys.Date() - INICIO_ZAFRA_ACTUAL)     # zafra 2026/2027

# Mínimo de imágenes para confiar en un lote en el MES EN CURSO.
# Como ceiling adelanta un mes, el bin más nuevo por cohorte puede tener lotes
# con muy pocas imágenes, y una sola imagen mueve el promedio a la alza/baja
# sin ser representativa. Se exige al menos este número de imágenes en ese bin.
MIN_IMG_MES_CURSO <- 2

# Función de cutoff dinámico: deja pasar filas cuya EDAD_MES sea MENOR
# al tope de su cohorte (tope = meses transcurridos - offset del mes).
# Meses no listados en OFFSET_MES_COSECHA no se filtran (tope = NA).
aplicar_cutoff_dinamico <- function(df, meses_transcurridos,
                                    offsets = OFFSET_MES_COSECHA) {
  df %>%
    mutate(
      .off  = offsets[MES_COSECHA_AC],
      .tope = meses_transcurridos - .off
    ) %>%
    filter(is.na(.tope) | EDAD_MES < .tope) %>%
    select(-.off, -.tope)
}

# Filtro de mínimo de imágenes en el MES EN CURSO.
# El "mes en curso" de cada cohorte es el bin más nuevo que dejó pasar el
# cutoff: EDAD_MES == meses_transcurridos - offset - 1.
# Descarta un lote (ID) SOLO si: es la temporada objetivo (la que está en
# curso), es su bin de mes en curso, y tiene menos de min_img imágenes (N_IMG).
# Los meses anteriores y las temporadas históricas no se tocan.
# Requiere que el data.frame ya tenga la columna N_IMG (= n() del summarise).
filtrar_mes_curso <- function(comp_grouped, meses_transcurridos, temporada_objetivo,
                              offsets = OFFSET_MES_COSECHA, min_img = MIN_IMG_MES_CURSO) {
  comp_grouped %>%
    ungroup() %>%
    mutate(
      .mes_curso = meses_transcurridos - offsets[MES_COSECHA_AC] - 1,
      .drop_flag = coalesce(
        TEMPORADA == temporada_objetivo &
          EDAD_MES == .mes_curso &
          N_IMG < min_img,
        FALSE
      )
    ) %>%
    filter(!.drop_flag) %>%
    select(-.mes_curso, -.drop_flag)
}

# --------------------------------------------------------------------
# Helpers de performance: version vectorizada de 3 patrones que en el
# script original eran for(...) { subset(); rbind() } por cada valor unico
# de un grupo (EDAD_MES, ID o ID2). Con miles de lotes/imagenes ese patron
# es O(n^2) (cada vuelta escanea el data.frame completo). Aca se resuelve
# con group_by()/mutate()/filter(), que hace un solo paso agrupado.
# Logica y resultados verificados como identicos al loop original
# (mismos valores; el orden de filas puede diferir, ver notas de export).
# --------------------------------------------------------------------

# Reemplaza el loop de filtro IQR (outliers -> NA) agrupado por EDAD_MES.
clip_outliers_iqr <- function(x, mult = 2) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  ifelse(x > q1 - iqr * mult & x < q3 + iqr * mult, x, NA)
}

# Reemplaza el loop que construye comparativa1: para cada ID conserva las
# filas de `datos` con EDAD_IMAGEN menor al maximo EDAD_IMAGEN de la
# temporada objetivo (+4). Los ID sin filas en la temporada objetivo se
# descartan por completo (equivalente al `next` del loop original).
construir_ventana_comparativa <- function(datos, temporada_objetivo) {
  datos %>%
    group_by(ID) %>%
    filter(any(TEMPORADA == temporada_objetivo)) %>%
    mutate(.umbral_edad = max(EDAD_IMAGEN[TEMPORADA == temporada_objetivo]) + 4) %>%
    filter(EDAD_IMAGEN < .umbral_edad) %>%
    ungroup() %>%
    select(-.umbral_edad)
}

# Reemplaza el loop de filtro IQR por fila (excluye filas, no las marca NA),
# agrupado por ID2.
filtrar_outliers_iqr_por_grupo <- function(df, grupo, valor, mult) {
  df %>%
    group_by({{ grupo }}) %>%
    mutate(
      .q1  = quantile({{ valor }}, 0.25, na.rm = TRUE),
      .q3  = quantile({{ valor }}, 0.75, na.rm = TRUE),
      .iqr = .q3 - .q1,
      .ls  = .q3 + .iqr * mult,
      .li  = .q1 - .iqr * mult
    ) %>%
    filter({{ valor }} >= .li & {{ valor }} <= .ls) %>%
    select(-.q1, -.q3, -.iqr, -.ls, -.li) %>%
    ungroup()
}

# --------------------------------------------------------------------
# 0) Parámetros de negocio
# --------------------------------------------------------------------
MAX_DAYS_AFTER_CUT_FOR_RESET <- 90    # siembra <= 90 días post-corte reinicia INICIO
MIN_MONTHS_START_END         <- 7.5   # edad mínima INICIO->FIN (meses)
HARVEST_WINDOW_MONTHS        <- c(11,12,1,2,3,4,5)  # ventana de cosecha (nov–may)
SEARCH_WINDOW_YEARS_AHEAD    <- 6     # horizonte de búsqueda de ventana factible

# --------------------------------------------------------------------
# 1) Helpers
# --------------------------------------------------------------------

# Año de zafra desde FECHA DE COSECHA (FIN):
#   Nov-Dic => año(fecha); Ene-May => año(fecha)-1
season_year_from_harvest <- function(d) {
  if (is.numeric(d)) d <- as.Date(d, origin = "1970-01-01")  # blindaje
  ifelse(lubridate::month(d) >= 11, lubridate::year(d), lubridate::year(d) - 1)
}

# Año de zafra desde FECHA DE INICIO:
# Siembras/inicios entre Jun-Oct del año X se cosechan en Nov(X)-May(X+1) → zafra X/X+1
# Inicios entre Nov-May ya están en ventana de cosecha → usar lógica de harvest
season_year_from_start <- function(inicio, fecha_cosecha_esperada) {
  if (is.numeric(inicio)) inicio <- as.Date(inicio, origin = "1970-01-01")
  if (is.numeric(fecha_cosecha_esperada)) fecha_cosecha_esperada <- as.Date(fecha_cosecha_esperada, origin = "1970-01-01")

  mes_inicio <- lubridate::month(inicio)
  ano_inicio <- lubridate::year(inicio)

  # Regla: Si inicio está en Jun-Oct (meses 6-10), la zafra es del año de inicio
  # porque se cosechará en la ventana Nov(año)-May(año+1)
  # Si inicio está en Nov-May (meses 11,12,1-5), usar la fecha de cosecha esperada
  ifelse(
    mes_inicio >= 6 & mes_inicio <= 10,  # Jun-Oct
    ano_inicio,                           # zafra = año de inicio
    season_year_from_harvest(fecha_cosecha_esperada)  # Nov-May: usar cosecha
  )
}

label_temporada <- function(ano_zafra) {
  ifelse(is.na(ano_zafra), NA, sprintf("%d/%d", ano_zafra, ano_zafra + 1))
}

# INICIO:
# si siembra <= 90d post-corte y siembra >= corte -> siembra; si no -> último corte; si falta -> la disponible
compute_inicio <- function(fecha_siembra, fecha_ult_corte, max_days_after_cut = 90) {
  d <- case_when(
    !is.na(fecha_siembra) & !is.na(fecha_ult_corte) &
      (as.numeric(difftime(fecha_siembra, fecha_ult_corte, units = "days")) <= max_days_after_cut) &
      (fecha_siembra >= fecha_ult_corte) ~ fecha_siembra,
    !is.na(fecha_ult_corte) ~ fecha_ult_corte,
    !is.na(fecha_siembra) ~ fecha_siembra,
    TRUE ~ as.Date(NA)
  )
  as.Date(d)
}

# Suma "meses" fraccionarios (~30.4375 d/mes) para 7.5, etc.
add_months_frac <- function(d, m_frac) {
  whole <- floor(m_frac)
  frac  <- m_frac - whole
  d2    <- d %m+% months(whole)
  as.Date(d2 + days(round(frac * 30.4375)))
}

# 1ª fecha de cosecha factible posterior a start_date:
#  - Dentro de nov 1 .. may 31 de alguna campaña
#  - Cumple >= MIN_MONTHS_START_END desde INICIO
find_first_feasible_harvest <- function(start_date,
                                        min_months = MIN_MONTHS_START_END,
                                        search_years_ahead = SEARCH_WINDOW_YEARS_AHEAD) {
  if (is.na(start_date)) return(as.Date(NA))
  min_date_needed <- add_months_frac(start_date, min_months)

  for (yy in (year(start_date)-1):(year(start_date) + search_years_ahead)) {
    window_start <- as.Date(sprintf("%04d-11-01", yy))
    window_end   <- as.Date(sprintf("%04d-05-31", yy + 1))
    candidate    <- max(min_date_needed, start_date, window_start)
    if (candidate <= window_end) return(as.Date(candidate))
  }
  as.Date(NA)
}

# Vectorizada + asegura clase Date
find_feasible_vec <- function(starts, min_months = MIN_MONTHS_START_END) {
  out_num <- vapply(
    as.list(starts),
    function(s) {
      x <- find_first_feasible_harvest(s, min_months)
      if (is.na(x)) NA_real_ else as.numeric(x)
    },
    numeric(1)
  )
  as.Date(out_num, origin = "1970-01-01")
}

# --------------------------------------------------------------------
# 2) Normalización de entradas (asume data.frames: historico, m_lote)
# --------------------------------------------------------------------

historico_norm <- historico %>%
  rename_with(~str_replace_all(., "\\s+", "_")) %>%
  mutate(
    FECHA_SIEMBRA        = as.Date(FECHA_SIEMBRA),
    FECHA_ULTIMO_CORTE   = as.Date(FECHA_ULTIMO_CORTE),
    FECHA_FINALIZO_CORTE = as.Date(FECHA_FINALIZO_CORTE),
    ANO_ZAFRA            = suppressWarnings(as.integer(ANO_ZAFRA)),
    FUENTE               = "historico"
  )

if (!"ACTIVO" %in% names(m_lote)) m_lote$ACTIVO <- ""

m_lote_norm <- m_lote %>%
  rename_with(~str_replace_all(., "\\s+", "_")) %>%
  mutate(
    FECHA_SIEMBRA        = as.Date(FECHA_SIEMBRA),
    FECHA_ULTIMO_CORTE   = as.Date(FECHA_ULTIMO_CORTE),
    FECHA_FINALIZO_CORTE = as.Date(FECHA_FINALIZO_CORTE),
    ACTIVO               = toupper(trimws(ACTIVO)),
    FUENTE               = "m_lote"
  )

# --------------------------------------------------------------------
# 3) HISTÓRICO: cálculo + validaciones
# --------------------------------------------------------------------

historico_calc <- historico_norm %>%
  mutate(
    INICIO_TEMP = compute_inicio(FECHA_SIEMBRA, FECHA_ULTIMO_CORTE, MAX_DAYS_AFTER_CUT_FOR_RESET),
    FIN_TEMP    = FECHA_FINALIZO_CORTE,
    # Zafra etiquetada por FECHA DE COSECHA (FIN)
    ANO_ZAFRA_CALC = if_else(!is.na(FIN_TEMP), season_year_from_harvest(FIN_TEMP), as.integer(NA)),
    TEMPORADA      = label_temporada(ANO_ZAFRA_CALC),
    EN_CURSO       = is.na(FIN_TEMP),
    REGLA_INICIO = case_when(
      !is.na(FECHA_SIEMBRA) & !is.na(FECHA_ULTIMO_CORTE) &
        (as.numeric(difftime(FECHA_SIEMBRA, FECHA_ULTIMO_CORTE, units="days")) <= MAX_DAYS_AFTER_CUT_FOR_RESET) &
        (FECHA_SIEMBRA >= FECHA_ULTIMO_CORTE) ~ "siembra<=90d_post_corte",
      !is.na(FECHA_ULTIMO_CORTE) ~ "ultimo_corte",
      !is.na(FECHA_SIEMBRA) ~ "siembra_sin_corte_prev",
      TRUE ~ "sin_base_fecha"
    )
  ) %>%
  group_by(COD_FINCA, COD_SECTOR, COD_LOTE) %>%
  arrange(INICIO_TEMP, .by_group = TRUE) %>%
  mutate(
    FIN_PREV                = lag(FIN_TEMP),
    INICIO_ES_CORTE_PREV    = !is.na(FIN_PREV) & !is.na(INICIO_TEMP) & (INICIO_TEMP == FIN_PREV),
    INICIO_ES_SIEMBRA_RESET = REGLA_INICIO == "siembra<=90d_post_corte",
    HIST_INICIO_VALIDO      = INICIO_ES_CORTE_PREV | INICIO_ES_SIEMBRA_RESET,
    EDAD_MESES_INICIO_FIN   = if_else(!is.na(INICIO_TEMP) & !is.na(FIN_TEMP),
                                      interval(INICIO_TEMP, FIN_TEMP) / months(1), NA_real_),
    HIST_MIN_7P5M_OK        = if_else(!is.na(EDAD_MESES_INICIO_FIN) & EDAD_MESES_INICIO_FIN >= MIN_MONTHS_START_END,
                                      TRUE, FALSE, missing = FALSE),
    HIST_FUERA_VENTANA      = if_else(!is.na(FIN_TEMP) & !(month(FIN_TEMP) %in% HARVEST_WINDOW_MONTHS),
                                      TRUE, FALSE, missing = FALSE),
    HIST_ANO_ZAFRA_MATCH    = if_else(!is.na(ANO_ZAFRA) & !is.na(ANO_ZAFRA_CALC),
                                      ANO_ZAFRA == ANO_ZAFRA_CALC, NA)
  ) %>%
  ungroup() %>%
  mutate(CREADA_DESDE_M_LOTE_PROX = FALSE)

# Último FIN real por lote (para anclar el INICIO de m_lote si falta/retrocede)
ultimo_fin_by_lote <- historico_calc %>%
  group_by(COD_FINCA, COD_SECTOR, COD_LOTE) %>%
  summarise(ULT_FIN_TEMP = suppressWarnings(max(FIN_TEMP, na.rm = TRUE)), .groups = "drop") %>%
  mutate(ULT_FIN_TEMP = ifelse(is.infinite(ULT_FIN_TEMP), as.Date(NA), as.Date(ULT_FIN_TEMP)))

# Última zafra por lote (para empujar m_lote a la próxima si colisiona)
last_zafra_by_lote <- historico_calc %>%
  group_by(COD_FINCA, COD_SECTOR, COD_LOTE) %>%
  summarise(ULT_ANO_ZAFRA = suppressWarnings(max(ANO_ZAFRA_CALC, na.rm = TRUE)), .groups = "drop") %>%
  mutate(ULT_ANO_ZAFRA = ifelse(is.infinite(ULT_ANO_ZAFRA), NA, ULT_ANO_ZAFRA))

# --------------------------------------------------------------------
# 4) M_LOTE (temporada en curso) con guardarraíl del último FIN histórico
# --------------------------------------------------------------------

# Separar registros CON y SIN fecha de finalización de corte
m_lote_con_fin <- m_lote_norm %>%
  filter(ACTIVO %in% c("S",""), !is.na(FECHA_FINALIZO_CORTE))

m_lote_sin_fin <- m_lote_norm %>%
  filter(ACTIVO %in% c("S",""), is.na(FECHA_FINALIZO_CORTE))

# ---- A) Procesar los que YA TIENEN FIN (cerrar temporada actual) ----
if (nrow(m_lote_con_fin) > 0) {
  m_lote_cerrada <- m_lote_con_fin %>%
    mutate(
      INICIO_TEMP_RAW = compute_inicio(FECHA_SIEMBRA, FECHA_ULTIMO_CORTE, MAX_DAYS_AFTER_CUT_FOR_RESET)
    ) %>%
    left_join(ultimo_fin_by_lote, by = c("COD_FINCA","COD_SECTOR","COD_LOTE")) %>%
    mutate(
      # INICIO de la temporada que se está cerrando
      TMP_INICIO_NUM = pmax(as.numeric(INICIO_TEMP_RAW),
                            as.numeric(ULT_FIN_TEMP),
                            na.rm = TRUE),
      TMP_INICIO_NUM = replace(TMP_INICIO_NUM, is.infinite(TMP_INICIO_NUM), NA_real_),
      INICIO_TEMP    = as.Date(TMP_INICIO_NUM, origin = "1970-01-01"),

      # FIN de esta temporada = FECHA_FINALIZO_CORTE
      FIN_TEMP = FECHA_FINALIZO_CORTE,

      # Zafra basada en la fecha de FIN
      ANO_ZAFRA_CALC = season_year_from_harvest(FIN_TEMP),
      TEMPORADA      = label_temporada(ANO_ZAFRA_CALC),
      EN_CURSO       = FALSE,

      INICIO_TOMADO_DE = case_when(
        !is.na(INICIO_TEMP_RAW) & !is.na(INICIO_TEMP) & INICIO_TEMP == INICIO_TEMP_RAW ~ "m_lote",
        !is.na(ULT_FIN_TEMP)    & !is.na(INICIO_TEMP) & INICIO_TEMP == ULT_FIN_TEMP    ~ "ultimo_fin_hist",
        TRUE ~ "sin_base"
      ),

      CREADA_DESDE_M_LOTE_PROX = FALSE,
      SALTO_ZAFRA              = FALSE,
      CURSO_MIN_7P5M_OK        = if_else(!is.na(INICIO_TEMP) & !is.na(FIN_TEMP) &
                                           interval(INICIO_TEMP, FIN_TEMP) / months(1) >= MIN_MONTHS_START_END,
                                         TRUE, FALSE, missing = FALSE),
      FECHA_COSECHA_ESPERADA   = as.Date(NA),
      REGLA_INICIO = case_when(
        !is.na(FECHA_SIEMBRA) & !is.na(FECHA_ULTIMO_CORTE) &
          (as.numeric(difftime(FECHA_SIEMBRA, FECHA_ULTIMO_CORTE, units="days")) <= MAX_DAYS_AFTER_CUT_FOR_RESET) &
          (FECHA_SIEMBRA >= FECHA_ULTIMO_CORTE) ~ "siembra<=90d_post_corte",
        !is.na(FECHA_ULTIMO_CORTE) ~ "ultimo_corte",
        !is.na(FECHA_SIEMBRA) ~ "siembra_sin_corte_prev",
        TRUE ~ "sin_base_fecha"
      )
    ) %>%
    select(-ULT_FIN_TEMP, -TMP_INICIO_NUM, -INICIO_TEMP_RAW)

  # ---- Crear la SIGUIENTE temporada (2026/2027) para esos lotes ----
  m_lote_siguiente <- m_lote_cerrada %>%
    mutate(
      # Guardar valores antes de limpiar
      TEMP_FECHA_CORTE   = FECHA_FINALIZO_CORTE,
      TEMP_FECHA_SIEMBRA = FECHA_SIEMBRA,

      # Evaluar si la siembra es válida para la NUEVA temporada
      # (debe ser posterior al corte y dentro de 90 días)
      SIEMBRA_VALIDA_NUEVA_TEMP = !is.na(TEMP_FECHA_SIEMBRA) &
        !is.na(TEMP_FECHA_CORTE) &
        TEMP_FECHA_SIEMBRA > TEMP_FECHA_CORTE &
        as.numeric(difftime(TEMP_FECHA_SIEMBRA, TEMP_FECHA_CORTE, units = "days")) <= MAX_DAYS_AFTER_CUT_FOR_RESET,

      # Determinar el INICIO de la nueva temporada
      INICIO_TEMP = if_else(SIEMBRA_VALIDA_NUEVA_TEMP, TEMP_FECHA_SIEMBRA, TEMP_FECHA_CORTE),

      # Preservar FECHA_SIEMBRA solo si es válida para la nueva temporada, sino limpiarla
      FECHA_SIEMBRA = if_else(SIEMBRA_VALIDA_NUEVA_TEMP, TEMP_FECHA_SIEMBRA, as.Date(NA)),

      # Limpiar otras fechas que ya fueron usadas en la temporada anterior
      FECHA_ULTIMO_CORTE = as.Date(NA),
      FECHA_FINALIZO_CORTE = as.Date(NA),
      FIN_TEMP    = as.Date(NA),

      FECHA_COSECHA_ESPERADA = find_feasible_vec(INICIO_TEMP, MIN_MONTHS_START_END),
      ANO_ZAFRA_CALC         = season_year_from_start(INICIO_TEMP, FECHA_COSECHA_ESPERADA),
      TEMPORADA              = label_temporada(ANO_ZAFRA_CALC),
      EN_CURSO               = TRUE,

      INICIO_TOMADO_DE       = if_else(SIEMBRA_VALIDA_NUEVA_TEMP, "siembra_post_corte", "fin_mlote"),
      CREADA_DESDE_M_LOTE_PROX = TRUE,
      SALTO_ZAFRA            = FALSE,
      CURSO_MIN_7P5M_OK      = if_else(!is.na(INICIO_TEMP) & !is.na(FECHA_COSECHA_ESPERADA) &
                                         interval(INICIO_TEMP, FECHA_COSECHA_ESPERADA) / months(1) >= MIN_MONTHS_START_END,
                                       TRUE, FALSE, missing = FALSE),
      REGLA_INICIO           = if_else(SIEMBRA_VALIDA_NUEVA_TEMP,
                                       "siembra<=90d_post_corte",
                                       "inicio_post_corte_mlote")
    ) %>%
    select(-TEMP_FECHA_CORTE, -TEMP_FECHA_SIEMBRA, -SIEMBRA_VALIDA_NUEVA_TEMP)  # Eliminar variables temporales
} else {
  m_lote_cerrada   <- m_lote_norm[0,] %>% mutate(INICIO_TEMP = as.Date(NA), FIN_TEMP = as.Date(NA),
                                                 ANO_ZAFRA_CALC = NA_integer_, TEMPORADA = NA_character_,
                                                 EN_CURSO = NA, CREADA_DESDE_M_LOTE_PROX = NA,
                                                 SALTO_ZAFRA = NA, CURSO_MIN_7P5M_OK = NA,
                                                 FECHA_COSECHA_ESPERADA = as.Date(NA),
                                                 INICIO_TOMADO_DE = NA_character_, REGLA_INICIO = NA_character_)
  m_lote_siguiente <- m_lote_cerrada
}

# ---- B) Procesar los que NO TIENEN FIN (temporada en curso) ----
if (nrow(m_lote_sin_fin) > 0) {
  m_lote_abierta <- m_lote_sin_fin %>%
    mutate(
      INICIO_TEMP_RAW = compute_inicio(FECHA_SIEMBRA, FECHA_ULTIMO_CORTE, MAX_DAYS_AFTER_CUT_FOR_RESET),
      FIN_TEMP        = as.Date(NA)
    ) %>%
    left_join(ultimo_fin_by_lote, by = c("COD_FINCA","COD_SECTOR","COD_LOTE")) %>%
    mutate(
      TMP_INICIO_NUM = pmax(as.numeric(INICIO_TEMP_RAW),
                            as.numeric(ULT_FIN_TEMP),
                            na.rm = TRUE),
      TMP_INICIO_NUM = replace(TMP_INICIO_NUM, is.infinite(TMP_INICIO_NUM), NA_real_),
      INICIO_TEMP_INICIAL = as.Date(TMP_INICIO_NUM, origin = "1970-01-01"),

      # DETECCIÓN DE RESIEMBRA: Si hay FECHA_SIEMBRA y está >90 días después del INICIO calculado
      # esto indica un RESET (nueva siembra que reinicia la temporada)
      DIAS_DIFF_SIEMBRA_INICIO = if_else(!is.na(FECHA_SIEMBRA) & !is.na(INICIO_TEMP_INICIAL),
                                         as.numeric(difftime(FECHA_SIEMBRA, INICIO_TEMP_INICIAL, units = "days")),
                                         NA_real_),

      HAY_RESIEMBRA = !is.na(DIAS_DIFF_SIEMBRA_INICIO) & DIAS_DIFF_SIEMBRA_INICIO > MAX_DAYS_AFTER_CUT_FOR_RESET,

      # APLICAR CORRECCIÓN: Si hay resiembra, usar FECHA_SIEMBRA como nuevo INICIO
      INICIO_TEMP = if_else(HAY_RESIEMBRA, FECHA_SIEMBRA, INICIO_TEMP_INICIAL),

      INICIO_TOMADO_DE = case_when(
        HAY_RESIEMBRA ~ "siembra_reset",
        !is.na(INICIO_TEMP_RAW) & !is.na(INICIO_TEMP) & INICIO_TEMP == INICIO_TEMP_RAW ~ "m_lote",
        !is.na(ULT_FIN_TEMP)    & !is.na(INICIO_TEMP) & INICIO_TEMP == ULT_FIN_TEMP    ~ "ultimo_fin_hist",
        TRUE ~ "sin_base"
      ),

      FECHA_COSECHA_ESPERADA = find_feasible_vec(INICIO_TEMP, MIN_MONTHS_START_END),
      ANO_ZAFRA_ESTIMADA     = season_year_from_start(INICIO_TEMP, FECHA_COSECHA_ESPERADA)
    ) %>%
    left_join(last_zafra_by_lote, by = c("COD_FINCA","COD_SECTOR","COD_LOTE")) %>%
    mutate(
      AJUSTA_PROX              = !is.na(ULT_ANO_ZAFRA) & !is.na(ANO_ZAFRA_ESTIMADA) & ANO_ZAFRA_ESTIMADA <= ULT_ANO_ZAFRA,
      ANO_ZAFRA_CALC           = if_else(AJUSTA_PROX, ULT_ANO_ZAFRA + 1L, ANO_ZAFRA_ESTIMADA),
      TEMPORADA                = label_temporada(ANO_ZAFRA_CALC),
      EN_CURSO                 = TRUE,
      CREADA_DESDE_M_LOTE_PROX = AJUSTA_PROX,
      SALTO_ZAFRA              = !is.na(ULT_ANO_ZAFRA) & !is.na(ANO_ZAFRA_CALC) & (ANO_ZAFRA_CALC - ULT_ANO_ZAFRA > 1),
      CURSO_MIN_7P5M_OK        = if_else(!is.na(INICIO_TEMP) & !is.na(FECHA_COSECHA_ESPERADA) &
                                           interval(INICIO_TEMP, FECHA_COSECHA_ESPERADA) / months(1) >= MIN_MONTHS_START_END,
                                         TRUE, FALSE, missing = FALSE),
      REGLA_INICIO = case_when(
        HAY_RESIEMBRA ~ "resiembra_reset_>90d",
        !is.na(FECHA_SIEMBRA) & !is.na(FECHA_ULTIMO_CORTE) &
          (as.numeric(difftime(FECHA_SIEMBRA, FECHA_ULTIMO_CORTE, units="days")) <= MAX_DAYS_AFTER_CUT_FOR_RESET) &
          (FECHA_SIEMBRA >= FECHA_ULTIMO_CORTE) ~ "siembra<=90d_post_corte",
        !is.na(FECHA_ULTIMO_CORTE) ~ "ultimo_corte",
        !is.na(FECHA_SIEMBRA) ~ "siembra_sin_corte_prev",
        TRUE ~ "sin_base_fecha"
      )
    ) %>%
    select(-ULT_ANO_ZAFRA, -AJUSTA_PROX, -ANO_ZAFRA_ESTIMADA, -TMP_INICIO_NUM, -ULT_FIN_TEMP,
           -INICIO_TEMP_RAW, -INICIO_TEMP_INICIAL, -DIAS_DIFF_SIEMBRA_INICIO, -HAY_RESIEMBRA)
} else {
  m_lote_abierta <- m_lote_norm[0,] %>% mutate(INICIO_TEMP = as.Date(NA), FIN_TEMP = as.Date(NA),
                                               ANO_ZAFRA_CALC = NA_integer_, TEMPORADA = NA_character_,
                                               EN_CURSO = NA, CREADA_DESDE_M_LOTE_PROX = NA,
                                               SALTO_ZAFRA = NA, CURSO_MIN_7P5M_OK = NA,
                                               FECHA_COSECHA_ESPERADA = as.Date(NA),
                                               INICIO_TOMADO_DE = NA_character_, REGLA_INICIO = NA_character_)
}

# ---- UNIFICAR: cerradas + siguientes + abiertas ----
m_lote_calc <- bind_rows(m_lote_cerrada, m_lote_siguiente, m_lote_abierta)


# --------------------------------------------------------------------
# 5) Unificación (prioriza histórico) y orden
# --------------------------------------------------------------------

cal_cols <- c(
  "COD_FINCA","COD_SECTOR","COD_LOTE", "LOTE_SEMILLERO", "AREA", "AREA_CULTIVO", "AREA_CORTADA", "COD_VARIEDAD",
  "INICIO_TEMP","FIN_TEMP","ANO_ZAFRA_CALC","TEMPORADA",
  "EN_CURSO","REGLA_INICIO","FUENTE",
  "FECHA_SIEMBRA","FECHA_ULTIMO_CORTE","FECHA_FINALIZO_CORTE",
  "CREADA_DESDE_M_LOTE_PROX",
  # Validaciones histórico
  "EDAD_MESES_INICIO_FIN","HIST_MIN_7P5M_OK","HIST_FUERA_VENTANA",
  "INICIO_ES_CORTE_PREV","INICIO_ES_SIEMBRA_RESET","HIST_INICIO_VALIDO",
  "HIST_ANO_ZAFRA_MATCH",
  # m_lote
  "FECHA_COSECHA_ESPERADA","CURSO_MIN_7P5M_OK","SALTO_ZAFRA","INICIO_TOMADO_DE"
)

calendario_temp <- bind_rows(
  historico_calc %>% select(any_of(cal_cols)),
  m_lote_calc   %>% select(any_of(cal_cols))
) %>%
  mutate(ORD = ifelse(FUENTE == "historico", 0L, 1L)) %>%
  arrange(COD_FINCA, COD_SECTOR, COD_LOTE, ANO_ZAFRA_CALC, ORD) %>%
  distinct(COD_FINCA, COD_SECTOR, COD_LOTE, ANO_ZAFRA_CALC, .keep_all = TRUE) %>%
  select(-ORD) %>%
  rename(ANO_ZAFRA = ANO_ZAFRA_CALC) %>%
  arrange(COD_FINCA, COD_SECTOR, COD_LOTE, INICIO_TEMP)

resultado_calendario <- calendario_temp

resultado_calendario$DIF <- as.double(resultado_calendario$FIN_TEMP-resultado_calendario$INICIO_TEMP)
resultado_calendario <- subset(resultado_calendario, is.na(DIF) | DIF <= 540)
hist(resultado_calendario$DIF)

# --------------------------------------------------------------------
# 6) Solapes dentro de un lote
# --------------------------------------------------------------------
resultado_solapes <- calendario_temp %>%
  group_by(COD_FINCA, COD_SECTOR, COD_LOTE) %>%
  arrange(INICIO_TEMP, .by_group = TRUE) %>%
  mutate(FIN_ANT = lag(FIN_TEMP)) %>%
  ungroup() %>%
  mutate(ALERTA_SOLAPE = if_else(!is.na(FIN_ANT) & !is.na(INICIO_TEMP) & INICIO_TEMP <= FIN_ANT, TRUE, FALSE)) %>%
  select(COD_FINCA, COD_SECTOR, COD_LOTE, ANO_ZAFRA, TEMPORADA,
         INICIO_TEMP, FIN_TEMP, FIN_ANT, ALERTA_SOLAPE, FUENTE)

# --------------------------------------------------------------------
# 7) Exportar a Excel
# --------------------------------------------------------------------
ruta_salida <- "salida.xlsx"

write_xlsx(
  list(
    "calendario" = resultado_calendario,
    "solapes"    = resultado_solapes
  ),
  path = ruta_salida
)

# Vistas rápidas (opcional)
print(head(resultado_calendario, 10))
print(head(dplyr::filter(resultado_solapes, ALERTA_SOLAPE), 10))

# ==========================================================
# Asignar TEMPORADA / ANO_ZAFRA y EDAD a registros fechados
# ==========================================================
asignar_edad_temporada <- function(df_imagenes, calendario, unidad = c("dias","meses"),
                                   modo = c("estricto","laxo_siguiente")) {
  unidad <- match.arg(unidad)
  modo   <- match.arg(modo)

  stopifnot(all(c("COD_FINCA","COD_SECTOR","COD_LOTE","FECHA_IMAGEN") %in% names(df_imagenes)))
  stopifnot(all(c("COD_FINCA","COD_SECTOR","COD_LOTE","INICIO_TEMP","ANO_ZAFRA","TEMPORADA", "LOTE_SEMILLERO", "AREA", "AREA_CULTIVO", "AREA_CORTADA", "COD_VARIEDAD") %in% names(calendario)))

  suppressPackageStartupMessages({
    library(dplyr); library(lubridate)
  })

  # ---------- Normalización de tipos ----------
  df2 <- df_imagenes %>%
    mutate(
      across(c(COD_FINCA, COD_SECTOR, COD_LOTE), ~as.character(.)),
      FECHA_IMAGEN = as.Date(FECHA_IMAGEN),
      .rowid = dplyr::row_number()
    )

  cal2 <- calendario %>%
    mutate(
      across(c(COD_FINCA, COD_SECTOR, COD_LOTE), ~as.character(.)),
      INICIO_TEMP = as.Date(INICIO_TEMP),
      FIN_TEMP    = as.Date(FIN_TEMP)
    ) %>%
    filter(!is.na(INICIO_TEMP)) %>%
    arrange(COD_FINCA, COD_SECTOR, COD_LOTE, INICIO_TEMP) %>%
    group_by(COD_FINCA, COD_SECTOR, COD_LOTE) %>%
    mutate(
      NEXT_INICIO = lead(INICIO_TEMP),
      LIMITE_FIN  = case_when(
        !is.na(FIN_TEMP)               ~ FIN_TEMP,
        is.na(FIN_TEMP) & !is.na(NEXT_INICIO) ~ NEXT_INICIO - days(1),
        TRUE                           ~ as.Date("9999-12-31")
      )
    ) %>%
    ungroup()

  # ---------- Expansión por lote/temporadas ----------
  exp <- tryCatch({
    suppressWarnings(
      dplyr::left_join(df2, cal2, by = c("COD_FINCA","COD_SECTOR","COD_LOTE"))
    )
  }, error = function(e) {
    stop("Fallo el join. Revisa tipos de COD_* y fechas. Error: ", e$message)
  })

  if (nrow(exp) == 0L) {
    return(df2 %>% mutate(EDAD_DIAS = NA_real_, EDAD_MESES = NA_real_,
                          EDAD = NA_real_, ANO_ZAFRA = NA_integer_, TEMPORADA = NA_character_,
                          INICIO_TEMP = as.Date(NA), FIN_TEMP = as.Date(NA),
                          SIN_TEMPORADA = TRUE) %>%
             select(-.rowid))
  }

  exp <- exp %>%
    mutate(
      STRICT  = !is.na(FECHA_IMAGEN) & !is.na(INICIO_TEMP) &
        FECHA_IMAGEN >= INICIO_TEMP & FECHA_IMAGEN <= LIMITE_FIN,
      AFTER_START = !is.na(FECHA_IMAGEN) & !is.na(INICIO_TEMP) & FECHA_IMAGEN >= INICIO_TEMP,
      DELTA_NEXT  = as.numeric(INICIO_TEMP - FECHA_IMAGEN)  # >0 si la temporada empieza después de la imagen
    )

  # ---------- 1) Preferir match estricto ----------
  chosen_strict <- exp %>%
    filter(STRICT) %>%
    group_by(.rowid) %>%
    slice_max(order_by = INICIO_TEMP, n = 1, with_ties = FALSE) %>%
    ungroup()

  # ---------- 2) Si no hay estricto y modo = laxo_siguiente: tomar la próxima temporada ----------
  if (modo == "laxo_siguiente") {
    missing_ids <- setdiff(df2$.rowid, chosen_strict$.rowid)

    chosen_next <- exp %>%
      filter(.rowid %in% missing_ids, !is.na(DELTA_NEXT), DELTA_NEXT > 0) %>%
      group_by(.rowid) %>%
      slice_min(order_by = DELTA_NEXT, n = 1, with_ties = FALSE) %>%  # INICIO más cercano hacia adelante
      ungroup()
  } else {
    chosen_next <- exp[0,]
  }

  chosen <- bind_rows(chosen_strict, chosen_next)

  # ---------- 3) Edades ----------
  chosen <- chosen %>%
    mutate(
      EDAD_DIAS  = as.numeric(FECHA_IMAGEN - INICIO_TEMP),
      EDAD_MESES = interval(INICIO_TEMP, FECHA_IMAGEN) / months(1)
    ) %>%
    select(.rowid, ANO_ZAFRA, TEMPORADA, LOTE_SEMILLERO, AREA, AREA_CULTIVO, AREA_CORTADA, INICIO_TEMP, FIN_TEMP, EDAD_DIAS, EDAD_MESES)

  # ---------- 4) Resultado final ----------
  out <- df2 %>%
    left_join(chosen, by = ".rowid") %>%
    mutate(
      EDAD = if (unidad == "dias") EDAD_DIAS else EDAD_MESES,
      SIN_TEMPORADA = is.na(TEMPORADA)
    ) %>%
    select(-.rowid)

  out
}

# --- NORMALIZACION CODIGOS FINCA (fix Occidente): antes 888,1010,1002 -----
# Rastunya II (888->5012), Belgica (1010->5018), La Cuchilla B (1002->5017)
resultado_calendario$TEMPORADA <- ifelse(
  !is.na(resultado_calendario$FECHA_SIEMBRA) &
    resultado_calendario$COD_FINCA %in% c(5012,5018,5017) &
    resultado_calendario$FECHA_SIEMBRA > as.Date("2025-08-21") &
    resultado_calendario$FECHA_SIEMBRA < as.Date("2025-10-21") &
    resultado_calendario$TEMPORADA == "2025/2026",
  "2026/2027",
  resultado_calendario$TEMPORADA
)



#Sys.setenv(LANGUAGE="es")
if(require(RJDBC)==FALSE){install.packages("RJDBC",dependencies = TRUE)}
library(dplyr)
driver <- RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver","C:/driver/ojdbc7.jar")
conexion <- dbConnect(driver, "jdbc:oracle:thin:@IMSAPST:1521/IMSAPSTIA","USR_INVES","sfDezcRHhC")
# NOTA: esta query se deja con SELECT * a proposito (no aplica la misma
# optimizacion que las otras 3). El `indices[,1:16]` de mas abajo asume
# EXACTAMENTE el orden de columnas fisico que devuelve la vista; convertir
# esto a columnas explicitas sin conocer ese orden podria reordenar/perder
# columnas y romper el pipeline en silencio (sin error, con datos mal
# etiquetados). Requiere confirmar antes el orden real de columnas de
# SDEUSR.VW_INDICE_VEGETACION.
query <- "SELECT * FROM SDEUSR.VW_INDICE_VEGETACION WHERE FECHA_IMAGEN >= TO_DATE('2018-11-01', 'YYYY-MM-DD')"
indices <- dbGetQuery(conexion, query)
indices <- normalizar_finca(indices, crosswalk_fincas)                                   # <- fix
max(indices$FECHA_IMAGEN)
indices$FECHA_IMAGEN <- as.Date(indices$FECHA_IMAGEN)



out_dia <- asignar_edad_temporada(indices, resultado_calendario,
                                  unidad = "dias", modo = "estricto")


indices <- out_dia
indices$ZAFRA <- indices$TEMPORADA
indices$EDAD_IMAGEN <- indices$EDAD_DIAS
indices <- indices[,1:16]

indices <- distinct(indices)
indices$ETAPA_FENOLOGICA <-cut(indices$EDAD_IMAGEN,
                               breaks = c(0, 50, 120, 235, 300, 370),
                               labels=c("INICIACION",
                                        "MACOLLAMIENTO",
                                        "ELONGACION_I",
                                        "ELONGACION_II",
                                        "MADURACION"))

# NOTA: esta EDAD_MES es en bins de ~10 días y se usa SOLO para el filtro IQR
# de abajo (agrupa los outliers en decenas). Más abajo se recalcula EDAD_MES
# en bins reales de 30 días para el resto del análisis.
indices$EDAD_MES <- floor(as.numeric(indices$EDAD_IMAGEN)/10)+1
indices <- subset(indices, !is.na(ZAFRA) & EDAD_IMAGEN>0 & NDVI > 0)
indices <- subset(indices, EDAD_IMAGEN < 400)

# Filtro IQR (outliers -> NA) por bin de EDAD_MES. Antes: for con
# subset()+rbind() por cada EDAD_MES unico (ver clip_outliers_iqr arriba).
# Misma logica, vectorizada por grupo.
indices_filtrados <- indices %>%
  group_by(EDAD_MES) %>%
  mutate(across(c(NDVI, NDWI, SAVI, EVI, MCARI, MM, IBR), ~clip_outliers_iqr(.x, mult = 2))) %>%
  ungroup() %>%
  as.data.frame()

colnames(indices_filtrados)[1] <- "TEMPORADA"
indices_filtrados$MM <- ifelse(indices_filtrados$EDAD_IMAGEN < 180, NA, indices_filtrados$MM)

if(require(RJDBC)==FALSE){install.packages("RJDBC",dependencies = TRUE)}
driver <- RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver","C:/driver/ojdbc7.jar")

conexion <- dbConnect(driver, "jdbc:oracle:thin:@IMSAPST:1521/IMSAPSTIA","USR_INVES","sfDezcRHhC")
query <- "SELECT ANO_ZAFRA, COD_FINCA, COD_SECTOR, COD_LOTE, LOTE, TAH, FECHA_CORTE, VARIEDAD, AREA, COD_VARIEDAD FROM SDEUSR.VW_ANALISIS_SACAROSA WHERE ANO_ZAFRA IN ('2023/2024', '2024/2025', '2025/2026')"
data_prod <- dbGetQuery(conexion, query)
data_prod <- normalizar_finca(data_prod, crosswalk_fincas)                               # <- fix
data_prod$TAH <- data_prod$TAH/1000
data_prod$LOTE <- as.numeric(data_prod$LOTE)
data_prod <- distinct(data_prod)

data1 <- subset(data_prod, ANO_ZAFRA %in% c("2024/2025", "2025/2026"))
data1$AZAFRA <- substr(data1$ANO_ZAFRA, 1,4)

data1 <- data1 %>%
  group_by(COD_FINCA, COD_SECTOR, COD_LOTE) %>%
  slice_max(AZAFRA)

MAESTRO <- data1[,c("COD_FINCA", "COD_SECTOR", "COD_LOTE", "FECHA_CORTE", "VARIEDAD", "AREA")]
MAESTRO <- distinct(MAESTRO)
colnames(MAESTRO) <- c("COD_FINCA","COD_SECTOR","COD_LOTE","FECHA_FINALIZO_CORTE","VARIEDAD_AC","AREA_AC")
MAESTRO$MES_COSECHA_AC <- format(as.Date(MAESTRO$FECHA_FINALIZO_CORTE), "%B")
MAESTRO$MES_COSECHA_AC <- ifelse(MAESTRO$MES_COSECHA_AC == "mayo", "abril", MAESTRO$MES_COSECHA_AC)
MAESTRO$FECHA_FINALIZO_CORTE <- NULL
indices_filtrados$COD_FINCA <- as.double(indices_filtrados$COD_FINCA)
indices_filtrados$COD_SECTOR <- as.double(indices_filtrados$COD_SECTOR)
indices_filtrados$COD_LOTE <- as.double(indices_filtrados$COD_LOTE)

VARIEDADES <- distinct(data_prod[,c("COD_VARIEDAD", "VARIEDAD")])

MAESTROA <- distinct(resultado_calendario[,c("TEMPORADA","COD_FINCA", "COD_SECTOR", "COD_LOTE", "COD_VARIEDAD", "AREA_CULTIVO", "INICIO_TEMP")])
MAESTROA <- MAESTROA %>% filter(TEMPORADA %in% c("2023/2024","2024/2025","2025/2026", "2026/2027"))
MAESTROA <- MAESTROA %>%
  group_by(COD_FINCA, COD_SECTOR, COD_LOTE) %>%
  slice_max(INICIO_TEMP)%>%select(-TEMPORADA) %>% distinct()
MAESTROA <- left_join(MAESTROA, VARIEDADES, by = "COD_VARIEDAD")

MAESTROA$MES_COSECHA_AC <- format(as.Date(MAESTROA$INICIO_TEMP), "%B")
MAESTROA$MES_COSECHA_AC <- ifelse(MAESTROA$MES_COSECHA_AC  %in% c("junio", "julio"), "mayo", MAESTROA$MES_COSECHA_AC)
MAESTROA$MES_COSECHA_AC <- ifelse(MAESTROA$MES_COSECHA_AC %in% c("agosto","octubre", "septiembre"), "noviembre", MAESTROA$MES_COSECHA_AC)
MAESTROA$INICIO_TEMP <- NULL
MAESTROA$COD_VARIEDAD <- NULL
MAESTROA <- MAESTROA[,c("COD_FINCA", "COD_SECTOR", "COD_LOTE", "VARIEDAD", "AREA_CULTIVO", "MES_COSECHA_AC")]
colnames(MAESTROA) <- c("COD_FINCA", "COD_SECTOR", "COD_LOTE", "VARIEDAD_AC", "AREA_AC", "MES_COSECHA_AC")

data <- left_join(indices_filtrados, MAESTROA, by = c("COD_FINCA", "COD_SECTOR", "COD_LOTE"))

data$ETAPA<-cut(data$EDAD_IMAGEN,
                breaks = c(0, 50, 120, 235, 300, 370, 390),
                labels=c("INICIACION",
                         "MACOLLAMIENTO",
                         "ELONGACION I",
                         "ELONGACION II",
                         "MADURACION",
                         "SR"))

data <- subset(data, !c(is.na(ETAPA) | ETAPA == "SR"))

data$EDAD_MES  <- floor(data$EDAD_IMAGEN/30)+1

datos <- subset(data, TEMPORADA %in% c("2023/2024", "2024/2025", "2025/2026", "2026/2027"))
datos$ID <- paste0(datos$COD_FINCA, "_", datos$COD_SECTOR, "_", datos$COD_LOTE)
datos_magdalena <- datos

#####################################################################################################
#TEMPORADA 2025/2026

datos <- subset(datos_magdalena, TEMPORADA %in% c("2023/2024", "2024/2025", "2025/2026"))
# Ventana comparativa por lote. Antes: for con subset()+rbind() por cada ID
# (ver construir_ventana_comparativa arriba). Misma logica: los ID sin
# filas en la temporada objetivo se descartan (FIX #2 preservado).
comparativa1 <- construir_ventana_comparativa(datos, "2025/2026")

comp1 <- comparativa1 %>%
  aplicar_cutoff_dinamico(meses_zafra_anterior) %>%          # <-- cutoff dinámico (reemplaza los 6 subset() manuales)
  group_by(TEMPORADA, MES_COSECHA_AC, EDAD_MES, ID,
           VARIEDAD_AC, COD_FINCA) %>%
  summarise(NDVI_MAGDALENA = mean(NDVI, na.rm = T),
            NDWI_MAGDALENA = mean(NDWI, na.rm = T),
            MCARI_MAGDALENA = mean(MCARI, na.rm = T),
            EVI_MAGDALENA = mean(EVI, na.rm = T),
            SAVI_MAGDALENA = mean(SAVI, na.rm = T),
            MM = mean(MM, na.rm = T),
            IBR = mean(IBR, na.rm = T),
            AREA = mean(AREA_AC, na.rm = T),
            N_IMG = n(), .groups = "drop") %>%                       # N_IMG = imágenes por lote-mes
  filtrar_mes_curso(meses_zafra_anterior, "2025/2026") %>%          # exige >=2 imágenes en el mes en curso
  select(-N_IMG)                                                     # se quita antes del melt

comp1 <- melt(comp1, id.vars = c("TEMPORADA", "MES_COSECHA_AC", "EDAD_MES", "ID", "VARIEDAD_AC", "COD_FINCA", "AREA"))
comp1$ID2 <- paste0(comp1$TEMPORADA, "_", comp1$MES_COSECHA_AC, "_", comp1$EDAD_MES , "_", comp1$variable)
colnames(comp1)[9] <- "NDVI"

# Filtro IQR por ID2. Antes: for con subset()+rbind() por cada ID2 unico
# (ver filtrar_outliers_iqr_por_grupo arriba). Misma logica, vectorizada.
datos_filtro <- filtrar_outliers_iqr_por_grupo(comp1, ID2, NDVI, mult = 3)

driver <- RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver","C:/driver/ojdbc7.jar")
conexion <- dbConnect(driver, "jdbc:oracle:thin:@IMSAPST:1521/IMSAPSTIA","USR_INVES","sfDezcRHhC")
lotes_activos <- dbGetQuery(conexion,"select region, finca, c_finca from sdeusr.lotes_imsa_gis")
datosf <- distinct(lotes_activos)
colnames(datosf)[3] <- "COD_FINCA"
datosf$COD_FINCA <- as.double(datosf$COD_FINCA)
datosf <- normalizar_finca(datosf, crosswalk_fincas, solo_finca = TRUE)                   # <- fix


datos_filtro <- inner_join(datos_filtro, datosf, by = "COD_FINCA" )
colnames(datos_filtro)[1] <- "ANO_ZAFRA"
colnames(datos_filtro)[5] <- "VARIEDAD"
datos_filtro$ID2 <- datos_filtro$variable
datos_filtro_magdalena_a <- datos_filtro[,c("ANO_ZAFRA", "MES_COSECHA_AC", "EDAD_MES", "ID", "COD_FINCA", "REGION", "FINCA", "VARIEDAD", "NDVI", "AREA", "ID2")]


#####################################################################################################
#TEMPORADA 2026/2027

datos <- subset(datos_magdalena, TEMPORADA %in% c("2023/2024", "2024/2025", "2025/2026","2026/2027"))
# Ventana comparativa por lote (ver nota equivalente arriba, bloque 2025/2026).
comparativa1 <- construir_ventana_comparativa(datos, "2026/2027")

comp1 <- comparativa1 %>%
  aplicar_cutoff_dinamico(meses_zafra_actual) %>%           # <-- cutoff dinámico (reemplaza los 6 subset() manuales)
  group_by(TEMPORADA, MES_COSECHA_AC, EDAD_MES, ID,
           VARIEDAD_AC, COD_FINCA) %>%
  summarise(NDVI_MAGDALENA = mean(NDVI, na.rm = T),
            NDWI_MAGDALENA = mean(NDWI, na.rm = T),
            MCARI_MAGDALENA = mean(MCARI, na.rm = T),
            EVI_MAGDALENA = mean(EVI, na.rm = T),
            SAVI_MAGDALENA = mean(SAVI, na.rm = T),
            MM = mean(MM, na.rm = T),
            IBR = mean(IBR, na.rm = T),
            AREA = mean(AREA_AC, na.rm = T),
            N_IMG = n(), .groups = "drop") %>%                       # N_IMG = imágenes por lote-mes
  filtrar_mes_curso(meses_zafra_actual, "2026/2027") %>%            # exige >=2 imágenes en el mes en curso
  select(-N_IMG)                                                     # se quita antes del melt

comp1 <- melt(comp1, id.vars = c("TEMPORADA", "MES_COSECHA_AC", "EDAD_MES", "ID", "VARIEDAD_AC", "COD_FINCA", "AREA"))
comp1$ID2 <- paste0(comp1$TEMPORADA, "_", comp1$MES_COSECHA_AC, "_", comp1$EDAD_MES , "_", comp1$variable)
colnames(comp1)[9] <- "NDVI"

# Filtro IQR por ID2 (ver nota equivalente arriba, bloque 2025/2026).
datos_filtro <- filtrar_outliers_iqr_por_grupo(comp1, ID2, NDVI, mult = 2)


datos_filtro <- inner_join(datos_filtro, datosf, by = "COD_FINCA" )
colnames(datos_filtro)[1] <- "ANO_ZAFRA"
colnames(datos_filtro)[5] <- "VARIEDAD"
datos_filtro$ID2 <- datos_filtro$variable
datos_filtro_magdalena_b <- datos_filtro[,c("ANO_ZAFRA", "MES_COSECHA_AC", "EDAD_MES", "ID", "COD_FINCA", "REGION", "FINCA", "VARIEDAD", "NDVI", "AREA", "ID2")]


datos_filtro_magdalena_a$ANALISIS <- "2025/2026"
datos_filtro_magdalena_b$ANALISIS <- "2026/2027"

datos_filtro_magdalena <- rbind(datos_filtro_magdalena_a,datos_filtro_magdalena_b)


############################################################################################################################
############################################################################################################################

write.csv(MAESTROA, "//CSCTFLDT/Investigacion/NDVI_CORREGIDO/DATOS_SEGUIMIENTO_CULTIVO/MAESTRO.csv",
          row.names = FALSE, na = "")

datos_filtro_union <- datos_filtro_magdalena
datos_filtro_union$ID2 <- as.character(datos_filtro_union$ID2)

datos_filtro_union$ID2 <- ifelse(datos_filtro_union$ID2 == "NDVI_MAGDALENA", "NDVI MAGDALENA", datos_filtro_union$ID2)
datos_filtro_union$ID2 <- ifelse(datos_filtro_union$ID2 == "NDWI_MAGDALENA", "NDWI MAGDALENA", datos_filtro_union$ID2)
datos_filtro_union$ID2 <- ifelse(datos_filtro_union$ID2 == "MCARI_MAGDALENA", "MCARI MAGDALENA", datos_filtro_union$ID2)
datos_filtro_union$ID2 <- ifelse(datos_filtro_union$ID2 == "EVI_MAGDALENA", "EVI MAGDALENA", datos_filtro_union$ID2)
datos_filtro_union$ID2 <- ifelse(datos_filtro_union$ID2 == "SAVI_MAGDALENA", "SAVI MAGDALENA", datos_filtro_union$ID2)

# --- Marcar fincas nuevas / sin historico (ruido en la curva BX) ---
# Lista fija segun data/fincas_nuevas.csv (columna ES_NUEVA, definida a mano por
# el ingenio en "Comparacion de Codigos Casa Elvira.xlsx"), no un calculo derivado:
# esas fincas no tienen zafras cerradas previas contra las cuales comparar, asi que
# su comparativa NDVI (promedio contra temporadas anteriores) no es representativa todavia.
fincas_nuevas <- read_csv("data/fincas_nuevas.csv", show_col_types = FALSE) %>%
  mutate(COD_FINCA = as.double(COD_FINCA))

fincas_sin_historico <- fincas_nuevas %>% filter(ES_NUEVA) %>% pull(COD_FINCA)

datos_filtro_union$SIN_HISTORICO <- datos_filtro_union$COD_FINCA %in% fincas_sin_historico
datos_filtro_union$COD_FINCA <- NULL   # se quita del CSV: Power BI ya la deriva de ID (split), evita choque de nombres

write.csv(datos_filtro_union, "//CSCTFLDT/Investigacion/NDVI_CORREGIDO/DATOS_SEGUIMIENTO_CULTIVO/DATOS_BX_NDVI.csv",
          row.names = FALSE, na = "")

########################################################################################################################################
########################################################################################################################################
dmg <- datos_magdalena[,c("TEMPORADA", "COD_FINCA", "COD_SECTOR", "COD_LOTE", "FECHA_IMAGEN", "EDAD_IMAGEN","ETAPA",
                          "MES_COSECHA_AC","VARIEDAD_AC", "AREA_AC", "NDVI", "NDWI", "MCARI", "EVI", "SAVI", "IBR")]

dmg <- melt(dmg, id.vars = c("TEMPORADA", "COD_FINCA", "COD_SECTOR", "COD_LOTE", "FECHA_IMAGEN", "EDAD_IMAGEN","ETAPA",
                             "MES_COSECHA_AC","VARIEDAD_AC", "AREA_AC"))

dpuntos <- dmg
dpuntos <- subset(dpuntos, !is.na(value))
dpuntos$variable <- as.character(dpuntos$variable)
dpuntos$variable <- ifelse(dpuntos$variable == "NDVI", "NDVI MAGDALENA", dpuntos$variable)
dpuntos$variable <- ifelse(dpuntos$variable == "NDWI", "NDWI MAGDALENA", dpuntos$variable)
dpuntos$variable <- ifelse(dpuntos$variable == "MCARI", "MCARI MAGDALENA", dpuntos$variable)
dpuntos$variable <- ifelse(dpuntos$variable == "EVI", "EVI MAGDALENA", dpuntos$variable)
dpuntos$variable <- ifelse(dpuntos$variable == "SAVI", "SAVI MAGDALENA", dpuntos$variable)
dpuntos <- inner_join(dpuntos, datosf, by = "COD_FINCA" )
dpuntos$LOTE <-  as.numeric(paste0(dpuntos$COD_FINCA,
                                   "0",
                                   dpuntos$COD_SECTOR,
                                   ifelse(dpuntos$COD_LOTE >9 , dpuntos$COD_LOTE, paste0("0", dpuntos$COD_LOTE))))

# dias_izafra / dias_izafra2 ahora se derivan de los parámetros de arriba
# (antes estaban hardcodeados como "2024-11-03" y "2025-11-07")
dpuntos <- dpuntos %>%
  filter(!(TEMPORADA == "2025/2026" & MES_COSECHA_AC == "noviembre" & EDAD_IMAGEN > dias_izafra)) %>%
  filter(!(TEMPORADA == "2025/2026" & MES_COSECHA_AC == "diciembre" & EDAD_IMAGEN > dias_izafra - 30)) %>%
  filter(!(TEMPORADA == "2025/2026" & MES_COSECHA_AC == "enero"    & EDAD_IMAGEN > dias_izafra - 60)) %>%
  filter(!(TEMPORADA == "2025/2026" & MES_COSECHA_AC == "febrero"  & EDAD_IMAGEN > dias_izafra - 90)) %>%
  filter(!(TEMPORADA == "2025/2026" & MES_COSECHA_AC == "marzo"    & EDAD_IMAGEN > dias_izafra - 120)) %>%
  filter(!(TEMPORADA == "2025/2026" & MES_COSECHA_AC == "abril"    & EDAD_IMAGEN > dias_izafra - 150)) %>%
  filter(!(TEMPORADA == "2025/2026" & MES_COSECHA_AC == "mayo"     & EDAD_IMAGEN > dias_izafra - 180))


dpuntos <- dpuntos %>%
  filter(!(TEMPORADA == "2026/2027" & MES_COSECHA_AC == "noviembre" & EDAD_IMAGEN > dias_izafra2)) %>%
  filter(!(TEMPORADA == "2026/2027" & MES_COSECHA_AC == "diciembre" & EDAD_IMAGEN > dias_izafra2 - 30)) %>%
  filter(!(TEMPORADA == "2026/2027" & MES_COSECHA_AC == "enero"    & EDAD_IMAGEN > dias_izafra2 - 60)) %>%
  filter(!(TEMPORADA == "2026/2027" & MES_COSECHA_AC == "febrero"  & EDAD_IMAGEN > dias_izafra2 - 90)) %>%
  filter(!(TEMPORADA == "2026/2027" & MES_COSECHA_AC == "marzo"    & EDAD_IMAGEN > dias_izafra2 - 120)) %>%
  filter(!(TEMPORADA == "2026/2027" & MES_COSECHA_AC == "abril"    & EDAD_IMAGEN > dias_izafra2 - 150)) %>%
  filter(!(TEMPORADA == "2026/2027" & MES_COSECHA_AC == "mayo"     & EDAD_IMAGEN > dias_izafra2 - 180))


write.csv(dpuntos, "//CSCTFLDT/Investigacion/NDVI_CORREGIDO/DATOS_SEGUIMIENTO_CULTIVO/DATOS_INDICES.csv",
          row.names = FALSE, na = "")


########################################################################################################################################
########################################################################################################################################

# FIX #6: check_region blindado (no truena si REGION ya existe o COD_FINCA es character)
check_region <- function(df, etiqueta) {
  cat("\n====", etiqueta, "====\n")
  if (!"MES_COSECHA_AC" %in% names(df)) {
    cat("(omitido: este data.frame aún no tiene la columna MES_COSECHA_AC)\n")
    return(invisible(NULL))
  }
  if (!"REGION" %in% names(df)) {
    tmp <- df %>%
      mutate(COD_FINCA = as.double(COD_FINCA)) %>%
      left_join(datosf, by = "COD_FINCA")
  } else {
    tmp <- df
  }
  print(table(tmp$REGION, tmp$MES_COSECHA_AC))
}

check_region(indices_filtrados, "Después de indices_filtrados")
check_region(comp1, "Después de comp1 (antes de IQR)")
check_region(datos_filtro, "Después de filtro IQR")
check_region(dpuntos, "Después de filtro dias_izafra")
