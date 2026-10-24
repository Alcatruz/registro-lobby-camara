# =============================================================================
# 02_indicadores.R — agregados para el post. Solo se publican conteos y
# porcentajes; ni nombres de quienes piden audiencia ni de diputados.
#
# Se corre desde lobbying_chile/post_registro_camara/, después de 01_base.R:
#   Rscript R/02_indicadores.R
#
# DECISIONES
#   - Serie anual de 2015 a 2026. 2014 queda fuera de la serie (32 audiencias,
#     desde diciembre) pero dentro de los totales. 2026 llega a septiembre.
#   - Oficinas = diputado por período con 20 o más audiencias. Se publican
#     sin identificador.
#   - Textos repetidos: se publican los 15 más frecuentes, revisados a mano
#     para que no contengan nombres de personas naturales.
#
# ESCRIBE
#   datos/escalera.csv, datos/serie_anual.csv, datos/por_tipo.csv,
#   datos/oficinas.csv, datos/textos_repetidos.csv, datos/datos.js
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})

b <- readRDS("_intermedio/audiencias_indicadores.rds")
dir.create("datos", showWarnings = FALSE)
p <- function(x) round(mean(x, na.rm = TRUE), 4)

# ---------------------------------------------------------------------------
# Escalera, de la audiencia registrada a la decisión identificada
# ---------------------------------------------------------------------------
escalera <- tibble(
  paso = c("Audiencias registradas", "Materia de 5 palabras o más",
           "Nombra una ley, un proyecto o un acto", "Se enlaza con un proyecto concreto",
           "Dice qué espera de esa decisión", "Cita el número de boletín"),
  n = c(nrow(b), sum(b$palabras >= 5), sum(b$nombra_decision), sum(b$proyecto_enlazado),
        sum(b$con_direccion), sum(b$cita_boletin))
) |> mutate(pct = round(n / nrow(b), 4))
print(as.data.frame(escalera))

# ---------------------------------------------------------------------------
# Serie anual
# ---------------------------------------------------------------------------
serie <- b |>
  filter(anno >= 2015) |>
  group_by(anno) |>
  summarise(audiencias = n(), mediana_palabras = median(palabras),
            cascaras = p(cascara), concreta = p(concreta),
            nombra_decision = p(nombra_decision), con_direccion = p(con_direccion),
            cita_boletin = p(cita_boletin), .groups = "drop")
print(as.data.frame(serie))

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
            cascaras = p(cascara), nombra_decision = p(nombra_decision),
            con_direccion = p(con_direccion), proyecto_enlazado = p(proyecto_enlazado),
            .groups = "drop") |>
  mutate(tipo = etiquetas[tipo_v2], pct_audiencias = round(audiencias / nrow(b), 4)) |>
  arrange(desc(audiencias))
print(as.data.frame(por_tipo))

# ---------------------------------------------------------------------------
# Oficinas (diputado por período, 20 o más audiencias), sin identificador
# ---------------------------------------------------------------------------
oficinas <- b |>
  filter(!is.na(id_diputado)) |>
  group_by(id_diputado, periodo) |>
  summarise(audiencias = n(), mediana_palabras = median(palabras),
            cascaras = p(cascara), nombra_decision = p(nombra_decision),
            con_direccion = p(con_direccion), .groups = "drop") |>
  filter(audiencias >= 20) |>
  arrange(periodo, nombra_decision) |>
  select(-id_diputado)
q <- function(x) round(quantile(x, c(0.1, 0.5, 0.9)), 3)
cat("\noficinas:", nrow(oficinas), "\n")
cat("nombra decisión p10 p50 p90:", q(oficinas$nombra_decision), "\n")
cat("cáscaras p10 p50 p90:", q(oficinas$cascaras), "| máx", max(oficinas$cascaras),
    "| con 50% o más:", sum(oficinas$cascaras >= 0.5), "\n")
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
            oficinas = n_distinct(diputado), .groups = "drop")
rep5 <- rep |> filter(audiencias >= 5)
cat("\ntextos usados 5 o más veces:", nrow(rep5), "| audiencias que cubren:", sum(rep5$audiencias),
    "(", round(100 * sum(rep5$audiencias) / nrow(b), 1), "%)\n")
cat("de esos textos, de un solo solicitante:", sum(rep5$solicitantes == 1),
    "| en dos o más oficinas:", sum(rep5$oficinas >= 2), "\n")
# texto original más frecuente para cada texto normalizado
original <- b |> count(m, materia) |> group_by(m) |> slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |> select(m, texto = materia)
top <- rep |> arrange(desc(audiencias)) |> head(15) |> left_join(original, by = "m") |>
  select(texto, audiencias, solicitantes, oficinas)
print(as.data.frame(top))

# ---------------------------------------------------------------------------
# Exportar
# ---------------------------------------------------------------------------
write.csv(escalera, "datos/escalera.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(serie, "datos/serie_anual.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(por_tipo, "datos/por_tipo.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(oficinas, "datos/oficinas.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(top, "datos/textos_repetidos.csv", row.names = FALSE, fileEncoding = "UTF-8")

datos <- list(
  total = nrow(b),
  desde = format(min(b$fecha)), hasta = format(max(b$fecha)),
  mediana_palabras = median(b$palabras),
  cascaras = p(b$cascara),
  repetidos = list(textos = nrow(rep5), audiencias = sum(rep5$audiencias),
                   un_solicitante = sum(rep5$solicitantes == 1),
                   varias_oficinas = sum(rep5$oficinas >= 2)),
  escalera = escalera, serie = serie,
  por_tipo = por_tipo |> select(tipo, audiencias, pct_audiencias, mediana_palabras, cascaras,
                                nombra_decision, con_direccion, proyecto_enlazado),
  oficinas = oficinas, textos = top
)
writeLines(c("// Generado por R/02_indicadores.R. No editar a mano.",
             paste0("window.LOBBY = ", toJSON(datos, auto_unbox = TRUE, dataframe = "rows"), ";")),
           "datos/datos.js", useBytes = TRUE)
cat("\nescrito datos/\n")
