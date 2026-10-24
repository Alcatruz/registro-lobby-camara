# Trece palabras

Qué dicen las 18.391 audiencias de lobby que registró la Cámara de Diputados de Chile entre diciembre de 2014 y septiembre de 2026.

El post está en `index.html` y se publica con GitHub Pages.

## Contenido

| Ruta | Qué es |
|---|---|
| `index.html` | El post, con gráficos interactivos (Plotly) |
| `datos/escalera.csv` | Audiencias según cuánto permite identificar la materia |
| `datos/serie_anual.csv` | Indicadores de la materia por año, 2015-2026 |
| `datos/por_tipo.csv` | Indicadores por tipo de solicitante |
| `datos/oficinas.csv` | Indicadores por oficina (diputado por período con 20 audiencias o más), sin identificador |
| `datos/textos_repetidos.csv` | Los 15 textos de materia más repetidos |
| `datos/datos.js` | Los datos que lee la página. Se genera con `R/02_indicadores.R` |
| `R/01_base.R` | Arma los indicadores por audiencia |
| `R/02_indicadores.R` | Calcula los agregados y exporta `datos/` |

## Definiciones

- **Cáscara.** Materia de 1 a 4 palabras.
- **Nombra una decisión.** La materia menciona una ley, un proyecto, vocabulario legislativo o un acto administrativo concreto (decreto, reglamento, resolución, licitación, permiso, concesión, subsidio, presupuesto, ordenanza, contrato, postulación).
- **Dice qué espera.** Nombra una decisión y además indica una dirección (aprobar, rechazar, modificar, derogar, indicaciones, entre otras).
- **Enlace con un proyecto concreto.** Boletín exacto o nombre del proyecto identificado con confianza alta.

Los scripts se corren desde esta carpeta y leen bases ya construidas a partir de las audiencias publicadas en camara.cl, que no están en el repositorio. Solo se publican agregados. No se publican nombres de quienes pidieron audiencias ni de diputados.
