# =============================================================================
# 02_infolobby.R — las mismas medidas, aplicadas a las 913.883 audiencias de
# InfoLobby, para comparar la Cámara con el resto del Estado.
#
# Se corre desde lobbying_chile/post_registro_camara/:
#   Rscript R/02_infolobby.R
#
# FUENTES (en lobbying_chile/, fuera del repositorio)
#   data/infolobby/audiencias_master_v6.csv   una fila por audiencia, armada
#                                             desde los datos abiertos de
#                                             InfoLobby (noviembre de 2014 a abril de 2026)
#   outputs/eda/clasificar_nivel.R            nivel territorial del organismo
#
# MEDIDAS (idénticas a 01_base.R, el texto es `observaciones`)
#   palabras, breve5 (5 o menos, incluye vacías), breve10, concreta (cifra o
#   norma), normativa, decision (instrumento identificable), verbos, boletin
#
# DECISIONES
#   - No se excluye ninguna audiencia. Las observaciones vacías cuentan 0 palabras.
#   - "Cámara en InfoLobby" es el organismo CAMARA DE DIPUTADOS dentro de la
#     base. Sirve para contrastar las dos fuentes, no entra aparte en los niveles.
#   - Antes de exportar se contrastan las cifras con las publicadas para el
#     sistema (mediana 19, 7,5% que identifica la decisión, 0,55% de boletín).
#
# ESCRIBE
#   datos/infolobby_grupos.csv   indicadores por nivel y para el total
#   datos/infolobby_tramos.csv   concreción por tramo de extensión
#   datos/infolobby_serie.csv    mediana de palabras por año
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

RAIZ <- ".."
f <- file.path(RAIZ, "data/infolobby/audiencias_master_v6.csv")
if (!file.exists(f)) stop("Correr desde post_registro_camara/.")
source(file.path(RAIZ, "outputs/eda/clasificar_nivel.R"))
dir.create("datos", showWarnings = FALSE)

aud <- read_csv(f, col_types = cols_only(organismo = col_character(),
                                          observaciones = col_character(),
                                          anio = col_integer()),
                na = character())
obs <- ifelse(is.na(aud$observaciones), "", str_squish(aud$observaciones))

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

aud <- aud |>
  mutate(
    nivel = as.character(clasificar_nivel(organismo)),
    palabras = if_else(obs == "", 0L, str_count(obs, "\\S+")),
    breve5 = palabras <= 5,
    breve10 = palabras <= 10,
    normativa = grepl(REG_NORMA, obs, ignore.case = TRUE),
    concreta = grepl("[0-9]", obs) | normativa,
    boletin = grepl(REG_BOLETIN, obs, ignore.case = TRUE),
    decision = boletin | grepl(REG_INSTRUMENTO, obs, ignore.case = TRUE),
    verbos = grepl(REG_VERBOS, obs, ignore.case = TRUE)
  )
cat("audiencias:", nrow(aud), "\n")

p <- function(x) round(mean(x), 4)
resumen <- function(d) {
  d |> summarise(audiencias = n(), mediana_palabras = median(palabras),
                 media_palabras = round(mean(palabras), 1),
                 breve5 = p(breve5), breve10 = p(breve10), concreta = p(concreta),
                 normativa = p(normativa), decision = p(decision), verbos = p(verbos),
                 boletin = p(boletin), .groups = "drop")
}

grupos <- bind_rows(
  resumen(aud) |> mutate(grupo = "InfoLobby, total"),
  resumen(group_by(aud, nivel)) |> mutate(grupo = paste0("InfoLobby, nivel ", tolower(nivel))) |> select(-nivel),
  resumen(filter(aud, toupper(organismo) == "CAMARA DE DIPUTADOS")) |> mutate(grupo = "Cámara en InfoLobby")
) |> relocate(grupo)
print.data.frame(as.data.frame(grupos))

# contraste con las cifras publicadas para el sistema
tot <- grupos[grupos$grupo == "InfoLobby, total", ]
chk <- c(mediana = tot$mediana_palabras == 19, breve5 = round(100 * tot$breve5, 1) == 20.2,
         decision = round(100 * tot$decision, 1) == 7.5, boletin = round(100 * tot$boletin, 2) == 0.55)
print(chk)
if (!all(chk)) warning("Las cifras no coinciden con las publicadas. Revisar antes de exportar.")

tramos <- aud |>
  filter(palabras > 0) |>
  mutate(tramo = cut(palabras, breaks = c(-Inf, 2, 5, 10, 19, 40, 100, Inf),
                     labels = c("1-2", "3-5", "6-10", "11-19", "20-40", "41-100", "100+"))) |>
  group_by(tramo) |>
  summarise(audiencias = n(), concreta = p(concreta), decision = p(decision), .groups = "drop") |>
  mutate(tramo = as.character(tramo))
print.data.frame(as.data.frame(tramos))

serie <- aud |>
  filter(anio >= 2015) |>
  group_by(anno = anio) |>
  summarise(audiencias = n(), mediana_palabras = median(palabras),
            concreta = p(concreta), decision = p(decision), .groups = "drop")
print.data.frame(as.data.frame(serie))

write.csv(grupos, "datos/infolobby_grupos.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(tramos, "datos/infolobby_tramos.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(serie, "datos/infolobby_serie.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\nescrito datos/infolobby_*.csv\n")
