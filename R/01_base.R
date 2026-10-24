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
#   legislativas_audiencias.rds  si la materia es legislativa (regla amplia)
#   audiencias_boletines.rds     audiencias cuya materia cita un boletín
#   trazabilidad.rds             proyecto de ley enlazado (exacto o alta)
#
# INDICADORES (mismas definiciones que el análisis de InfoLobby)
#   palabras      palabras de la materia (0 si está vacía)
#   cascara       entre 1 y 4 palabras
#   concreta      trae una cifra o una referencia normativa (ley, proyecto,
#                 decreto, reglamento, resolución, artículo, número)
#   nombra_decision  la materia es legislativa (regla amplia) o nombra un acto
#                 administrativo concreto (criterio de cumplimiento_materia.R)
#   con_direccion nombra una decisión y además dice qué se espera de ella
#                 (aprobar, rechazar, modificar, indicaciones, etc.)
#   cita_boletin  la materia trae un número de boletín
#   proyecto_enlazado  la audiencia se enlaza con un proyecto concreto
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
leg  <- readRDS(file.path(DIR, "legislativas_audiencias.rds")) |>
  select(id_audiencia, legislativa_amplia, legislativa_expl)
bol  <- readRDS(file.path(DIR, "audiencias_boletines.rds")) |> distinct(id_audiencia) |>
  mutate(cita_boletin = TRUE)
traz <- readRDS(file.path(DIR, "trazabilidad.rds")) |> select(id_audiencia, proyecto_enlazado, nivel)

normaliza <- function(x) {
  x |> str_to_lower() |> stringi::stri_trans_general("Latin-ASCII") |>
    str_replace_all("[^a-z0-9 ]", " ") |> str_squish()
}

b <- a |>
  left_join(tipo, by = "id_audiencia") |>
  left_join(leg,  by = "id_audiencia") |>
  left_join(bol,  by = "id_audiencia") |>
  left_join(traz, by = "id_audiencia") |>
  mutate(
    materia = coalesce(str_squish(materia), ""),
    m = normaliza(materia),
    palabras = if_else(materia == "", 0L, lengths(str_split(materia, " "))),
    cascara = palabras >= 1 & palabras <= 4,
    concreta = str_detect(materia, "[0-9]") |
      str_detect(m, "\\b(ley|proyecto|decreto|reglamento|resoluci|articulo|ds|n)\\b"),
    acto_administrativo = str_detect(m, "\\b(decreto|reglamento|resolucion|licitacion|permiso|concesion|subsidio|presupuesto|ordenanza|contrato|postulacion)"),
    legislativa_amplia = coalesce(legislativa_amplia, FALSE),
    nombra_decision = legislativa_amplia | acto_administrativo,
    direccion = str_detect(m, "\\b(aprob|rechaz|modific|derog|elimin|agreg|incorpor|inclu|exclu|amplia|extender|extension|posterg|acelerar|urgencia|votar|voto a favor|voto en contra|en contra de|a favor de|indicacion|indicaciones|patrocin|suspender|prorrog|reponer|retirar)"),
    con_direccion = nombra_decision & direccion,
    cita_boletin = coalesce(cita_boletin, FALSE),
    proyecto_enlazado = coalesce(proyecto_enlazado, FALSE),
    tipo_v2 = coalesce(tipo_v2, "sin_clasificar"),
    anno = as.integer(format(fecha, "%Y"))
  ) |>
  add_count(m, name = "n_repeticiones") |>
  mutate(n_repeticiones = if_else(m == "", NA_integer_, n_repeticiones))

cat("vacías:", sum(b$palabras == 0), "| cáscaras:", sum(b$cascara),
    "| citan boletín:", sum(b$cita_boletin), "| proyecto enlazado:", sum(b$proyecto_enlazado), "\n")
cat("sin tipo de solicitante:", sum(b$tipo_v2 == "sin_clasificar"), "\n")

saveRDS(b, "_intermedio/audiencias_indicadores.rds")
cat("escrito _intermedio/audiencias_indicadores.rds\n")
