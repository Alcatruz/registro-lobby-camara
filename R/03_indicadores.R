# =============================================================================
# 03_indicadores.R — agregados para el post. Solo se publican conteos y
# porcentajes; ni nombres de quienes piden audiencia ni de diputados.
#
# Se corre desde lobbying_chile/post_registro_camara/, después de 01_base.R y
# 02_infolobby.R:
#   Rscript R/03_indicadores.R
#
# DECISIONES
#   - Serie anual de 2015 a 2026. 2014 queda fuera de la serie (32 audiencias,
#     desde diciembre) pero dentro de los totales. 2026 llega a septiembre en
#     la Cámara y a abril en InfoLobby.
#   - Oficinas = diputado por período con 20 o más audiencias. Se publican
#     sin identificador.
#   - Textos repetidos: se publican los 15 más frecuentes, revisados a mano
#     para que no contengan nombres de personas naturales.
#
# ESCRIBE
#   datos/comparacion.csv, datos/tramos.csv, datos/serie_anual.csv,
#   datos/por_tipo.csv, datos/oficinas.csv, datos/textos_repetidos.csv,
#   datos/datos.js
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})

b <- readRDS("_intermedio/audiencias_indicadores.rds")
if (!file.exists("datos/infolobby_grupos.csv")) stop("Correr antes R/02_infolobby.R.")
ig <- read.csv("datos/infolobby_grupos.csv", fileEncoding = "UTF-8")
it <- read.csv("datos/infolobby_tramos.csv", fileEncoding = "UTF-8")
is <- read.csv("datos/infolobby_serie.csv", fileEncoding = "UTF-8")
p <- function(x) round(mean(x, na.rm = TRUE), 4)

# ---------------------------------------------------------------------------
# La Cámara frente a InfoLobby, con las mismas medidas
# ---------------------------------------------------------------------------
camara <- b |>
  summarise(audiencias = n(), mediana_palabras = median(palabras),
            media_palabras = round(mean(palabras), 1),
            breve5 = p(breve5), breve10 = p(breve10), concreta = p(concreta),
            normativa = p(normativa), decision = p(decision), verbos = p(verbos),
            boletin = p(boletin)) |>
  mutate(grupo = "Cámara (camara.cl)", .before = 1)
comparacion <- bind_rows(camara, ig)
print.data.frame(as.data.frame(comparacion))

# ---------------------------------------------------------------------------
# Concreción por tramo de extensión
# ---------------------------------------------------------------------------
tramos <- b |>
  filter(palabras > 0) |>
  mutate(tramo = cut(palabras, breaks = c(-Inf, 2, 5, 10, 19, 40, 100, Inf),
                     labels = c("1-2", "3-5", "6-10", "11-19", "20-40", "41-100", "100+"))) |>
  group_by(tramo) |>
  summarise(audiencias = n(), concreta = p(concreta), decision = p(decision), .groups = "drop") |>
  mutate(tramo = as.character(tramo), fuente = "Cámara") |>
  bind_rows(mutate(it, fuente = "InfoLobby"))
print.data.frame(as.data.frame(tramos))

# ---------------------------------------------------------------------------
# Serie anual
# ---------------------------------------------------------------------------
serie <- b |>
  filter(anno >= 2015) |>
  group_by(anno) |>
  summarise(audiencias = n(), mediana_palabras = median(palabras),
            breve5 = p(breve5), concreta = p(concreta), decision = p(decision),
            boletin = p(boletin), .groups = "drop") |>
  left_join(is |> select(anno, mediana_infolobby = mediana_palabras), by = "anno")
print.data.frame(as.data.frame(serie))

# ---------------------------------------------------------------------------
# Por tipo de solicitante
# ---------------------------------------------------------------------------
etiquetas <- c(empresa = "Empresas", persona_natural = "Personas naturales",
               sociedad_civil = "Sociedad civil", trabajadores = "Trabajadores",
               profesional = "Profesionales", territorial = "Organizaciones territoriales",
               otras = "Otras organizaciones", sin_clasificar = "Sin clasificar")
por_tipo <- b |>
  group_by(tipo_v2) |>
  summarise(audiencias = n(), mediana_palabras = median(palabras),
            breve5 = p(breve5), concreta = p(concreta), decision = p(decision),
            boletin = p(boletin), .groups = "drop") |>
  mutate(tipo = etiquetas[tipo_v2], pct_audiencias = round(audiencias / nrow(b), 4)) |>
  arrange(desc(audiencias))
print.data.frame(as.data.frame(por_tipo))

# ---------------------------------------------------------------------------
# Oficinas (diputado por período, 20 o más audiencias), sin identificador
# ---------------------------------------------------------------------------
oficinas <- b |>
  filter(!is.na(id_diputado)) |>
  group_by(id_diputado, periodo) |>
  summarise(audiencias = n(), mediana_palabras = median(palabras),
            breve5 = p(breve5), concreta = p(concreta), decision = p(decision),
            .groups = "drop") |>
  filter(audiencias >= 20) |>
  arrange(periodo, decision) |>
  select(-id_diputado)
q <- function(x) round(quantile(x, c(0.1, 0.5, 0.9)), 3)
cat("\noficinas:", nrow(oficinas), "\n")
cat("identifica la decisión p10 p50 p90:", q(oficinas$decision), "| mín", min(oficinas$decision),
    "| máx", max(oficinas$decision), "\n")
cat("concreta p10 p50 p90:", q(oficinas$concreta), "\n")
cat("5 palabras o menos p10 p50 p90:", q(oficinas$breve5), "| con 50% o más:", sum(oficinas$breve5 >= 0.5), "\n")
cat("mediana de palabras p10 p50 p90:", q(oficinas$mediana_palabras), "\n")

# ---------------------------------------------------------------------------
# Textos repetidos
# ---------------------------------------------------------------------------
# el nombre de quien pide se usa solo para contar solicitantes distintos
quien <- readRDS("../congreso_lobby/congreso_lobby/audiencias_camara_id.rds") |>
  distinct(id_audiencia, .keep_all = TRUE) |> select(id_audiencia, representado, diputado)
rep <- b |> filter(m != "") |> left_join(quien, by = "id_audiencia") |>
  group_by(m) |>
  summarise(audiencias = n(), solicitantes = n_distinct(representado),
            oficinas = n_distinct(diputado), decision = any(decision), .groups = "drop")
rep5 <- rep |> filter(audiencias >= 5)
cat("\ntextos usados 5 o más veces:", nrow(rep5), "| audiencias que cubren:", sum(rep5$audiencias),
    "(", round(100 * sum(rep5$audiencias) / nrow(b), 1), "%)\n")
cat("de esos textos, de un solo solicitante:", sum(rep5$solicitantes == 1),
    "| en dos o más oficinas:", sum(rep5$oficinas >= 2),
    "| identifican la decisión:", sum(rep5$decision), "\n")
# texto original más frecuente para cada texto normalizado
original <- b |> count(m, materia) |> group_by(m) |> slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |> select(m, texto = materia)
top <- rep |> arrange(desc(audiencias)) |> head(15) |> left_join(original, by = "m") |>
  select(texto, audiencias, solicitantes, oficinas)
print(as.data.frame(top))

# ---------------------------------------------------------------------------
# Exportar
# ---------------------------------------------------------------------------
write.csv(comparacion, "datos/comparacion.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(tramos, "datos/tramos.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(serie, "datos/serie_anual.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(por_tipo, "datos/por_tipo.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(oficinas, "datos/oficinas.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(top, "datos/textos_repetidos.csv", row.names = FALSE, fileEncoding = "UTF-8")

datos <- list(
  total = nrow(b),
  desde = format(min(b$fecha)), hasta = format(max(b$fecha)),
  repetidos = list(textos = nrow(rep5), audiencias = sum(rep5$audiencias),
                   un_solicitante = sum(rep5$solicitantes == 1),
                   varias_oficinas = sum(rep5$oficinas >= 2)),
  comparacion = comparacion, tramos = tramos, serie = serie,
  por_tipo = por_tipo |> select(tipo, audiencias, pct_audiencias, mediana_palabras, breve5,
                                concreta, decision, boletin),
  oficinas = oficinas, textos = top
)
writeLines(c("// Generado por R/03_indicadores.R. No editar a mano.",
             paste0("window.LOBBY = ", toJSON(datos, auto_unbox = TRUE, dataframe = "rows"), ";")),
           "datos/datos.js", useBytes = TRUE)
cat("\nescrito datos/\n")
