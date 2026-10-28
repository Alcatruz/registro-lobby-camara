# =============================================================================
# 01_base.R — una fila por audiencia de lobby de la Cámara con indicadores de
# cuánto dice la materia registrada.
#
# Se corre desde lobbying_chile/post_registro_camara/:
#   Rscript R/01_base.R
#
# FUENTES (congreso_lobby/congreso_lobby/, ya construidas)
#   audiencias_camara_id.rds     scrape de camara.cl, 18.391 audiencias,
#                                diciembre de 2014 a septiembre de 2026
#   representados_tipo_v2.rds    tipo de quien pide la audiencia
#
# MEDIDAS (las mismas que 02_infolobby.R aplica a InfoLobby, con el mismo código)
#   palabras    palabras de la materia (0 si está vacía)
#   breve5      5 palabras o menos
#   breve10     10 palabras o menos
#   concreta    menciona una cifra o una referencia normativa
#   normativa   menciona una referencia normativa (ley, proyecto, decreto,
#               reglamento, resolución, artículo, D.S., N°)
#   decision    identifica el instrumento sobre el que se gestiona (boletín,
#               proyecto de ley, indicación, moción, ley, decreto o resolución
#               con número, reglamento, ordenanza, licitación, tramitación)
#   verbos      trae un verbo de gestión (solicitar, proponer, apoyar,
#               oponerse, modificar, aprobar, entre otros)
#   boletin     menciona un boletín o un número con su forma (1234-05)
#   n_repeticiones  audiencias con exactamente el mismo texto normalizado
#
# ESCRIBE
#   _intermedio/audiencias_indicadores.rds   (no se publica: trae la materia)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

DIR <- "../congreso_lobby/congreso_lobby"
if (!file.exists(file.path(DIR, "audiencias_camara_id.rds"))) stop("Correr desde post_registro_camara/.")
dir.create("_intermedio", showWarnings = FALSE)

a <- readRDS(file.path(DIR, "audiencias_camara_id.rds")) |>
  distinct(id_audiencia, .keep_all = TRUE) |>
  select(id_audiencia, fecha, periodo, id_diputado, es_asesor, materia)
cat("audiencias:", nrow(a), "| rango", format(range(a$fecha, na.rm = TRUE)), "\n")

tipo <- readRDS(file.path(DIR, "representados_tipo_v2.rds")) |> select(id_audiencia, tipo_v2)

REG_NORMA   <- "\\bley\\b|proyecto|decreto|reglamento|resoluci|art.culo|d\\.s\\.|n[°º]"
REG_BOLETIN <- "bolet[ií]n|\\b\\d{4,5}\\s*-\\s*\\d{2}\\b"
REG_INSTRUMENTO <- paste0(
  "proyecto de ley|\\bpdl\\b|\\bindicaci[oó]n\\b|\\bmoci[oó]n\\b|",
  "\\bley\\s*(n[°º.]{0,2}\\s*)?\\d|",
  "decreto\\s*(supremo\\s*)?(n[°º.]{0,2}\\s*)?\\d|",
  "d\\.?s\\.?\\s*n?[°º.]?\\s*\\d|",
  "resoluci[oó]n\\s*(exenta\\s*)?(n[°º.]{0,2}\\s*)?\\d|",
  "\\breglamento\\b|\\bordenanza\\b|\\blicitaci[oó]n\\b|",
  "\\btramitaci[oó]n\\b|comisi[oó]n de hacienda|primer tr[aá]mite|segundo tr[aá]mite")
REG_VERBOS <- paste0(
  "solicit|pedir|pide|propon|propuest|plante|expon|apoy|respald|",
  "oponer|oposici[oó]n|rechaz|promov|impuls|gestion|requer|insist|",
  "defend|modific|derog|aprob|agiliz|acelerar|priorizar")

# texto normalizado, solo para contar textos repetidos
normaliza <- function(x) {
  x |> str_to_lower() |> stringi::stri_trans_general("Latin-ASCII") |>
    str_replace_all("[^a-z0-9 ]", " ") |> str_squish()
}

b <- a |>
  left_join(tipo, by = "id_audiencia") |>
  mutate(
    materia = coalesce(str_squish(materia), ""),
    m = normaliza(materia),
    palabras = if_else(materia == "", 0L, str_count(materia, "\\S+")),
    breve5 = palabras <= 5,
    breve10 = palabras <= 10,
    normativa = grepl(REG_NORMA, materia, ignore.case = TRUE),
    concreta = grepl("[0-9]", materia) | normativa,
    boletin = grepl(REG_BOLETIN, materia, ignore.case = TRUE),
    decision = boletin | grepl(REG_INSTRUMENTO, materia, ignore.case = TRUE),
    verbos = grepl(REG_VERBOS, materia, ignore.case = TRUE),
    tipo_v2 = coalesce(tipo_v2, "sin_clasificar"),
    anno = as.integer(format(fecha, "%Y"))
  ) |>
  add_count(m, name = "n_repeticiones") |>
  mutate(n_repeticiones = if_else(m == "", NA_integer_, n_repeticiones))

cat("vacías:", sum(b$palabras == 0), "| 5 palabras o menos:", sum(b$breve5),
    "| identifican la decisión:", sum(b$decision), "| citan boletín:", sum(b$boletin), "\n")
cat("sin tipo de solicitante:", sum(b$tipo_v2 == "sin_clasificar"), "\n")

saveRDS(b, "_intermedio/audiencias_indicadores.rds")
cat("escrito _intermedio/audiencias_indicadores.rds\n")
