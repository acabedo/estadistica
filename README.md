# Estadística básica con R para la investigación en ELE

Materiales del taller **Estadística básica con R para la investigación en ELE** (Máster en ELE, Universitat de Barcelona, 23 de octubre de 2026), impartido por Adrián Cabedo Nebot (Universitat de València). Coordinación: Dra. Carolina Figueras.

**Presentación en línea:** <https://acabedo.github.io/estadistica/>

La presentación está hecha con [Quarto](https://quarto.org) y reveal.js. Cada bloque sigue el mismo ciclo: **copia** el código en RStudio, **comprueba** que tu resultado coincide con el de la diapositiva y **explora** una diapositiva interactiva (D3 y plotly) que explica qué significa ese resultado.

## Contenidos

| Sesión | Tema | Qué se ve |
|---|---|---|
| 1 | R y estadística descriptiva | Objetos y vectores, tendencia central, dispersión, cuartiles, resúmenes por grupos con `dplyr` |
| 2 | Visualización | Gramática de gráficos con `ggplot2`, gráficos interactivos, *motion charts*, mapas de calor |
| 3 | Inferencia | Distribución normal, intervalos de confianza, valor p, potencia, tamaño del efecto, prueba t, Wilcoxon, correlación |
| 4 | Chi cuadrado y ANOVA | Tablas de contingencia, residuos, Fisher, ANOVA de uno y dos factores, Tukey, Kruskal-Wallis |
| 5 | Corpus y modelos | Log-likelihood, log-ratio, Simple Maths, `quanteda`, regresión lineal y logística |
| 🌳 | Árboles de decisión | Impureza de Gini, poda, sobreajuste, validación cruzada, matriz de confusión, árboles de regresión |

Cada sesión termina con una práctica de 20 minutos y sus soluciones.

## Uso de la presentación

Ábrela en el navegador. Atajos útiles:

- **→ / ←**: avanzar y retroceder
- **M**: menú lateral con todas las diapositivas
- **Esc**: vista general
- **C**: pizarra para anotar
- **S**: notas del ponente
- **E**: modo de impresión (para exportar a PDF desde el navegador)

Cada bloque de código tiene un botón para copiarlo.

## Datos

Los datos de `datos/` son **simulados** (no corresponden a aprendientes reales) y se generan con `datos/crear_datos.R` (semilla 2026):

- `ele.csv`: 240 aprendientes de ELE con L1, nivel (A2–C1), estancia en España, horas de estudio, nota DELE, errores de *ser/estar* y uso del subjuntivo.
- `errores.csv`: recuentos de tipos de error por L1 en un corpus de aprendices.
- `frecuencias.csv`: frecuencias de palabras en un corpus de aprendientes y en uno de nativos.

## Seguir el taller en tu ordenador

1. Instala [R](https://cran.r-project.org) y [RStudio](https://posit.co/download/rstudio-desktop/), o usa [posit.cloud](https://posit.cloud) sin instalar nada.
2. Crea un proyecto y copia dentro la carpeta `datos/`.
3. Instala los paquetes (solo una vez):

   ```r
   install.packages(c("tidyverse", "rpart", "rpart.plot",
                      "pheatmap", "plotly", "DT"))
   ```

   Para la sesión 5 y los ejemplos opcionales también se usan `quanteda`, `quanteda.textstats`, `quanteda.textplots`, `readtext` y `ranger`.

## Estructura del repositorio

```
├── index.html                          # Presentación renderizada (la que sirve GitHub Pages)
├── estadistica_ELE_interactiva.qmd     # Fuente de la presentación
├── estadistica_ELE_interactiva_files/  # Librerías y figuras generadas por Quarto
├── estilo_interactivo.css              # Estilos de la presentación
├── interactivo/                        # Componentes interactivos (D3) y datos que usan
│   ├── preparar_datos.R                #   genera datos_curso.js a partir de los análisis en R
│   └── incluir.html                    #   carga los scripts al final de la presentación
├── datos/                              # Datos simulados y script que los genera
└── img/                                # Imágenes de portada de cada sesión
```

## Volver a renderizar

Hacen falta [Quarto](https://quarto.org) (≥ 1.4), R y los paquetes anteriores, además de `jsonlite`.

```bash
quarto render estadistica_ELE_interactiva.qmd
mv estadistica_ELE_interactiva.html index.html
```

GitHub Pages publica `index.html` desde la raíz de la rama `main`. Después de renderizar, sube a la vez `index.html` y la carpeta `estadistica_ELE_interactiva_files/`, porque los nombres de algunos archivos cambian en cada renderizado.

## Licencia

[GNU GPL v3](LICENSE).

## Referencias recomendadas

- Cabedo Nebot, A. (2021). *Fundamentos de estadística con R para lingüistas*. Tirant Lo Blanch.
- Wickham, H., Çetinkaya-Rundel, M. y Grolemund, G. *R for Data Science* (2.ª ed.): <https://r4ds.hadley.nz>
- Winter, B. (2019). *Statistics for Linguists: An Introduction Using R*. Routledge.
- Gries, S. Th. (2021). *Statistics for Linguistics with R* (3.ª ed.). De Gruyter.
- LADAL, Language Technology and Data Analysis Laboratory: <https://ladal.edu.au>
