# Trece palabras

Qué dicen las 18.391 audiencias de lobby que registró la Cámara de Diputados de Chile entre diciembre de 2014 y septiembre de 2026, comparadas con las 913.883 que reúne InfoLobby para todo el Estado.

[Leer el análisis](https://alcatruz.github.io/registro-lobby-camara/)

El post está en `index.html` y se publica con GitHub Pages.

## Contenido

| Ruta | Qué es |
|---|---|
| `index.html` | El post, con gráficos interactivos (Plotly) |
| `datos/comparacion.csv` | Indicadores de la Cámara y de InfoLobby (total, por nivel territorial y la Cámara dentro de InfoLobby) |
| `datos/tramos.csv` | Concreción por tramo de extensión, Cámara e InfoLobby |
| `datos/serie_anual.csv` | Indicadores de la materia por año, 2015-2026 |
| `datos/por_tipo.csv` | Indicadores por tipo de solicitante |
| `datos/oficinas.csv` | Indicadores por oficina (diputado por período con 20 audiencias o más), sin identificador |
| `datos/textos_repetidos.csv` | Los 15 textos de materia más repetidos |
| `datos/infolobby_*.csv` | Agregados de InfoLobby que escribe `R/02_infolobby.R` |
| `datos/datos.js` | Los datos que lee la página. Se genera con `R/03_indicadores.R` |
| `R/01_base.R` | Arma los indicadores por audiencia de la Cámara |
| `R/02_infolobby.R` | Aplica las mismas medidas a InfoLobby |
| `R/03_indicadores.R` | Calcula los agregados y exporta `datos/` |

## Definiciones

Las mismas reglas se aplican, con el mismo código, a la materia de la Cámara y al campo de observaciones de InfoLobby.

- **Palabras.** Palabras separadas por espacios. Las materias breves son las de 5 palabras o menos.
- **Concreta.** Menciona una cifra o una referencia normativa (ley, proyecto, decreto, reglamento, resolución, artículo, D.S., N°).
- **Identifica la decisión.** Nombra el instrumento sobre el que se gestiona, un boletín, un proyecto de ley, una indicación, una moción, una ley, un decreto o una resolución con su número, un reglamento, una ordenanza, una licitación o una tramitación.
- **Cita el boletín.** Menciona un boletín o un número con su forma (1234-05).

Los scripts se corren desde esta carpeta, en este orden, y leen bases que no están en el repositorio. Las audiencias de camara.cl vienen de `../congreso_lobby/congreso_lobby/` y las de InfoLobby de `../data/infolobby/audiencias_master_v6.csv`. Solo se publican agregados. No se publican nombres de quienes pidieron audiencias ni de diputados.
