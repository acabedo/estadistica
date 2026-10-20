"""Genera los apuntes imprimibles (apuntes_estadistica_ELE.qmd → PDF) a partir de la presentación.

Lee estadistica_ELE_interactiva.qmd y convierte cada diapositiva en un apartado: título, texto,
código con su salida real y el texto «En pocas palabras» de las notas. Los componentes
interactivos se sustituyen por figuras o tablas estáticas (apuntes/figuras_apuntes.R).

Uso:  python3 apuntes/construir_apuntes.py   (desde la carpeta del curso) y después
      quarto render apuntes_estadistica_ELE.qmd
"""
import re
from pathlib import Path

RAIZ = Path(__file__).resolve().parent.parent
ORIGEN = RAIZ / "estadistica_ELE_interactiva.qmd"
DESTINO = RAIZ / "apuntes_estadistica_ELE.qmd"

# Diapositivas que solo tienen sentido en pantalla
OMITIR = {"Mapa del curso", "Cómo trabajaremos: copia → resultado → explica"}
OMITIR_HERO = {"inicio", "gracias"}

# ---------------------------------------------------------------- componentes → figuras estáticas
CAPAS = '''ggplot(ele, aes(x = horas, y = nota_dele,     # datos + estéticas x e y
                colour = estancia)) +         # tercera estética: el color
  geom_point(alpha = .6) +                    # capa de puntos
  geom_smooth(method = "lm", se = FALSE) +    # capa de rectas, una por grupo
  facet_wrap(~ nivel, nrow = 1) +             # un panel por nivel
  labs(x = "Horas de instrucción", y = "Nota DELE", colour = "Estancia")'''

SEGUIR = '''nuevos <- tibble(horas = c(300, 200, 400),
                 nivel = factor(c("B1", "A2", "C1"), levels = levels(ele$nivel)),
                 L1 = c("chino", "inglés", "italiano"),
                 estancia = c("no", "sí", "no"))
rpart.predict(arbol, nuevos, rules = TRUE)'''

SEGUIR_NOTA = '''predict(arbol_nota, nuevos)                  # nota prevista para cada aprendiente
rpart.predict(arbol_nota, nuevos, rules = TRUE)'''

W = {
    "muestra": [("fig_muestra()", 2.5)],
    "media-mediana": [("fig_media_mediana()", 2.7)],
    "dispersion": [("fig_dispersion()", 2.7)],
    "de-pasos": [("tabla_de_pasos()", None), ("fig_de_pasos()", 2.7)],
    "cuartiles": [("fig_cuartiles()", 2.5)],
    "resumen-grupos": [("fig_resumen_grupos()", 2.7)],
    "anscombe": [("fig_anscombe()", 2.3)],
    "capas": [(CAPAS, 2.8, "El gráfico completo, capa a capa")],
    "histograma": [("fig_histograma()", 2.3)],
    "geoms": [("fig_geoms()", 2.9)],
    "heatmap": [("fig_heatmap()", 2.7)],
    "elige-grafico": [("tabla_elige_grafico()", None)],
    "normal": [("fig_normal()", 2.5)],
    "muestral": [("fig_muestral()", 2.5)],
    "ic": [("fig_ic()", 3.0)],
    "valor-p": [("fig_valor_p()", 2.7)],
    "potencia": [("fig_potencia()", 2.7)],
    "cohen-d": [("fig_cohen_d()", 2.1)],
    "elige-prueba": [("tabla_elige_prueba()", None)],
    "senal-ruido": [("fig_senal_ruido()", 2.7)],
    "wilcoxon": [("tabla_wilcoxon()", None)],
    "correlacion": [("fig_correlacion()", 2.1)],
    "chi-mosaico": [("fig_chi()", 2.7)],
    "fwer": [("fig_fwer()", 2.7)],
    "anova-f": [("fig_anova_f()", 2.7)],
    "tukey": [("fig_tukey()", 2.7)],
    "interaccion": [("fig_interaccion()", 2.5)],
    "keyness": [("fig_keyness()", 3.1)],
    "simple-maths": [("fig_simple_maths()", 3.1)],
    "regresion": [("fig_regresion()", 2.9)],
    "logistica": [("fig_logistica()", 2.5), ("tabla_logistica()", None)],
    "anatomia-arbol": [("fig_anatomia_arbol()", 2.9)],
    "gini": [("fig_gini()", 2.7)],
    "mejor-corte": [("fig_mejor_corte()", 2.7)],
    "arbol/grande/crecer": [("tabla_crecer()", None)],
    "arbol/grande/explorar": [("tabla_reglas()", None)],
    "arbol/grande/seguir": [(SEGUIR, None, "Prueba esto en R: predecir para aprendientes nuevos")],
    "arbol/nota/seguir": [(SEGUIR_NOTA, None, "Prueba esto en R (usa los mismos tres aprendientes)")],
    "cp-curva": [("fig_cp()", 2.7)],
    "confusion": [("tabla_confusion()", None), ("tabla_matriz()", None)],
}

ANCHO = 6.3   # ancho de las figuras en pulgadas (A4 con márgenes de 2 cm)


def chunk(codigo, alto=None, etiqueta=None):
    """Bloque de R: oculto si es una figura o tabla de apoyo, visible si lleva etiqueta."""
    opts = []
    if etiqueta is None:
        opts.append("#| echo: false")
    if alto:
        opts += [f"#| fig-width: {ANCHO}", f"#| fig-height: {alto}"]
    cab = f"\n**{etiqueta}**\n\n" if etiqueta else ""
    return cab + "```{r}\n" + "\n".join(opts + [codigo]) + "\n```\n"


# ---------------------------------------------------------------- utilidades de texto
def html_a_md(t):
    t = re.sub(r"<b>(.*?)</b>", r"**\1**", t)
    t = re.sub(r"<i>(.*?)</i>", r"*\1*", t)
    t = re.sub(r"<code>(.*?)</code>", r"`\1`", t)
    t = re.sub(r"<br\s*/?>", " ", t)
    t = re.sub(r"</?[a-zA-Z][^>]*>", "", t)
    return t.strip()


def limpiar_linea(l):
    l = l.replace("✔ ", "").replace("✘ ", "")
    if l.startswith("- [ ] "):
        return "1. " + l[6:]
    if re.match(r"^(Pasa el ratón|Pulsa una tarjeta)", l):
        return ""
    l = re.sub(r"\[([^\]]+)\]\(#/[^)]+\)", r"\1", l)                 # enlaces internos de reveal
    m = re.match(r"^\[(.+)\]\{\.paso-et\}\s*$", l)
    if m:
        return f"**{m.group(1)}**"
    m = re.match(r'^<div class="grande">(.*)</div>\s*$', l)
    if m:
        return f"::: {{.center}}\n**{m.group(1)}**\n:::"
    m = re.match(r"^<details><summary>(.*?)</summary>(.*)</details>\s*$", l)
    if m:
        return f"*{m.group(1)}:* {m.group(2)}"
    if "<" in l and re.search(r"</?(div|span|a|p|b|i|br|code)\b", l):
        return html_a_md(l)
    return l


SUSTITUCIONES = [
    ("Las diapositivas", "Los apartados"), ("las diapositivas", "los apartados"),
    ("Esta diapositiva", "Este apartado"), ("esta diapositiva", "este apartado"),
    ("La diapositiva", "El apartado"), ("la diapositiva", "el apartado"),
    ("una diapositiva interactiva", "un gráfico interactivo"), ("diapositiva", "apartado"),
    (" de el ", " del "), (" a el ", " al "),
]


def para_papel(t):
    # Las frases que piden pasar el ratón o pulsar solo tienen sentido en pantalla
    t = re.sub(r"[^.\n]*(pasar el ratón|Pasa el ratón|pasa el ratón)[^.\n]*\. ?", "", t)
    for a, b in SUSTITUCIONES:
        t = t.replace(a, b)
    return t


# ---------------------------------------------------------------- análisis de bloques ::: y ```
class Div:
    def __init__(self, attrs):
        self.attrs, self.hijos = attrs, []
        m = re.match(r"\{?\s*\.?([\w-]+)", attrs)
        self.clase = m.group(1) if m else ""

    def attr(self, nombre):
        m = re.search(rf'{nombre}="([^"]*)"', self.attrs)
        return m.group(1) if m else None


def analizar(lineas):
    raiz = Div("raiz"); pila = [raiz]; i = 0
    while i < len(lineas):
        l = lineas[i]
        if l.startswith("```"):
            j = i + 1
            while not lineas[j].startswith("```"):
                j += 1
            pila[-1].hijos.append(("codigo", lineas[i:j + 1])); i = j + 1; continue
        m = re.match(r"^:{3,}\s*(\S.*)$", l)
        if m:
            d = Div(m.group(1).strip()); pila[-1].hijos.append(d); pila.append(d)
        elif re.match(r"^:{3,}\s*$", l):
            pila.pop()
        else:
            pila[-1].hijos.append(("texto", l))
        i += 1
    return raiz


def texto_de(div):
    return "\n".join(h[1] for h in div.hijos if isinstance(h, tuple) and h[0] == "texto")


# ---------------------------------------------------------------- conversión de bloques de código
def convertir_codigo(lineas):
    opts = [l for l in lineas[1:-1] if l.startswith("#|")]
    cuerpo = [l for l in lineas[1:-1] if not l.startswith("#|")]
    texto = "\n".join(cuerpo)
    o = {}
    for l in opts:
        m = re.match(r"#\|\s*([\w.-]+):\s*(.*)$", l)
        if m:
            o[m.group(1)] = m.group(2).strip().strip('"')
    # Sustituciones de salidas que solo existen en HTML
    if "DT::datatable" in texto:
        return chunk("knitr::kable(head(ele, 10))")
    if "ggplotly(p, height" in texto:
        return chunk("p", 3.2)
    if "motion |> layout" in texto:
        return chunk("fig_motion()", 3.2)
    nuevas = []
    if "plot_ly(" in texto:
        o["eval"] = "false"; o.pop("output", None)
    for k, v in o.items():
        if k in ("filename", "output-location", "fig-width", "fig-height"):
            continue
        nuevas.append(f"#| {k}: {v}")
    if "fig-width" in o or "fig-height" in o:
        w = float(o.get("fig-width", 9)); h = float(o.get("fig-height", 5.2))
        if "rpart.plot" in texto or "plotcp" in texto:
            nw, nh = ANCHO, min(4.6, max(3.2, h * ANCHO / w * 1.4))
        else:
            nw, nh = ANCHO, max(2.4, min(3.6, h * ANCHO / w))
        nuevas += [f"#| fig-width: {nw}", f"#| fig-height: {round(nh, 2)}"]
    cab = f"\n**{o['filename']}**\n\n" if "filename" in o else ""
    return cab + "```{r}\n" + "\n".join(nuevas + cuerpo) + "\n```\n"


# ---------------------------------------------------------------- conversión de bloques :::
def anatomia(div, indice):
    salida = next(h for h in div.hijos if isinstance(h, Div) and h.clase == "salida")
    partes = re.findall(r'\[((?:`[^`]*`|[^\[\]`])+?)\]\{\.an data-exp="([^"]*)"\}', texto_de(salida))
    bloque = chunk("tt" if indice == 0 else "arbol", None, None).replace("#| echo: false\n", "#| echo: false\n#| comment: \"\"\n")
    items = []
    for txt, exp in partes:
        txt = re.sub(r"\\\s*\n", " ", txt).replace("\\", "").strip()
        txt = re.sub(r"\s+", " ", txt)
        items.append(f"- `{txt}`" if "`r " not in txt else f"- {txt}")
        items.append(f"  {html_a_md(exp)}")
    return bloque + "\nCómo se lee cada parte de la salida:\n\n" + "\n".join(items) + "\n"


def tarjetas(div):
    out = []
    for l in texto_de(div).splitlines():
        m = re.match(r'^<div><span class="t">(.*?)</span>(.*)</div>\s*$', l.strip())
        if m:
            cuerpo = html_a_md(m.group(2).replace("<p", " <p").replace("<li>", " · "))
            titulo = html_a_md(m.group(1)).replace("✔", "").replace("✘", "").strip()
            out.append(f"- **{titulo}**: {cuerpo}")
    return "\n".join(out) + "\n"


def giro(div):
    out = []
    for l in texto_de(div).splitlines():
        m = re.search(r'class="fr">(.*?)</div><div class="re">(.*?)</div>', l)
        if m:
            out.append(f"- **{html_a_md(m.group(1))}** → {html_a_md(m.group(2))}")
    return "\n".join(out) + "\n"


def hub(div):
    out = []
    for l in texto_de(div).splitlines():
        m = re.search(r'class="t">(.*?)</span><span class="d">(.*?)</span>', l)
        if m:
            out.append(f"- **{m.group(1)}**: {m.group(2)}")
    return "\n".join(out) + "\n"


class Estado:
    anatomias = 0
    sesiones = 0


def render(nodo, est, notas):
    out = []
    for h in nodo.hijos:
        if isinstance(h, tuple):
            out.append(convertir_codigo(h[1]) if h[0] == "codigo" else limpiar_linea(h[1]))
            continue
        c = h.clase
        if c == "notes":
            exp = next((x for x in h.hijos if isinstance(x, Div) and x.clase == "explicacion"), None)
            if exp:
                notas.append(para_papel(texto_de(exp).strip()))
        elif c == "w":
            clave = h.attr("data-w")
            if clave == "arbol":
                clave = f"arbol/{h.attr('data-arbol')}/{h.attr('data-modo')}"
            for item in W[clave]:
                out.append(chunk(item[0], item[1], item[2] if len(item) > 2 else None))
        elif c == "tarjetas":
            out.append(tarjetas(h))
        elif c == "giro":
            out.append(giro(h))
        elif c == "hub":
            out.append(hub(h))
        elif c == "anatomia":
            out.append(anatomia(h, est.anatomias)); est.anatomias += 1
        elif c in ("idea", "clave", "recuadro"):
            titulo = {"idea": "Idea clave", "clave": "Para recordar", "recuadro": "En una frase"}[c]
            tipo = {"idea": "important", "clave": "tip", "recuadro": "warning"}[c]
            cuerpo = render(h, est, notas).replace("(Pasa el ratón o pulsa cada pregunta.)", "").strip()
            out.append(f'::: {{.callout-{tipo} title="{titulo}"}}\n{cuerpo}\n:::\n')
        elif c == "panel-tabset":
            sub = render(h, est, notas)
            out.append(re.sub(r"^### (.+)$", r"**\1**", sub, flags=re.M))
        else:   # columns, column, pequeno, practica…: se conserva el contenido
            out.append(render(h, est, notas))
    return "\n".join(out)


def hero(titulo_attrs, contenido):
    ident = re.search(r"#([\w-]+)", titulo_attrs).group(1)
    if ident in OMITIR_HERO:
        return ""
    kicker = re.search(r"\[(.*?)\]\{\.kicker\}", contenido).group(1)
    h1 = re.search(r"<h1>(.*?)</h1>", contenido, re.S).group(1).strip()
    preg = re.search(r"::: pregunta\n(.*?)\n:::", contenido, re.S)
    chips = re.findall(r"<span>(.*?)</span>", contenido)
    if kicker.startswith("Plus"):
        h1 = f"Plus: {h1}"
    out = f"# {h1}\n\n*{kicker}*\n\n"
    if preg:
        out += f"::: {{.callout-note title=\"La pregunta de esta sesión\"}}\n{preg.group(1).strip()}\n:::\n\n"
    if chips:
        out += "**Contenidos:** " + " · ".join(chips) + ".\n\n"
    return out


# ---------------------------------------------------------------- documento
CABECERA = r'''---
lang: es
format:
  typst:
    papersize: a4
    margin:
      x: 2cm
      y: 2.2cm
    fontsize: 10pt
    mainfont: "Helvetica Neue"
    number-sections: true
    fig-format: png
    fig-dpi: 220
    fig-width: 6.3
    fig-height: 3
    include-in-header: apuntes/estilo_apuntes.typ
execute:
  warning: false
  message: false
  echo: true
---

```{r}
#| include: false
library(tidyverse)
library(rpart)
library(rpart.plot)
library(plotly)
theme_set(theme_minimal(base_size = 10))
pal <- c("#1D3557", "#E63946", "#2A9D8F", "#F4A261")
ele <- read_csv("datos/ele.csv") |>
  mutate(nivel = factor(nivel, levels = c("A2", "B1", "B2", "C1")))
errores <- read_csv("datos/errores.csv")
frec <- read_csv("datos/frecuencias.csv")
fmt <- function(x, d = 2) format(round(x, d), nsmall = d, decimal.mark = ",", big.mark = ".")
options(width = 88)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
source("apuntes/figuras_apuntes.R")
```

```{=typst}
#page(numbering: none, footer: none)[
  #v(1.2cm)
  #block(width: 100%, fill: rgb("#1D3557"), inset: (x: 20pt, y: 22pt), radius: 8pt)[
    #text(fill: rgb("#F4A261"), size: 9pt, weight: "bold", tracking: 1.5pt)[APUNTES DEL CURSO]
    #v(4pt)
    #text(fill: white, size: 24pt, weight: "bold")[Estadística básica con R para la investigación en ELE]
    #v(2pt)
    #text(fill: rgb("#A8DADC"), size: 12pt)[De los datos del aula a las pruebas estadísticas]
  ]
  #v(10pt)
  #image("img/arbol_intuicion.jpg", width: 100%)
  #v(10pt)
  #grid(columns: (1fr, 1fr), gutter: 14pt,
    [#text(weight: "bold", fill: rgb("#1D3557"))[Adrián Cabedo Nebot] \ Universitat de València \ adrian.cabedo\@uv.es],
    [Máster en Español como Lengua Extranjera \ Universitat de Barcelona \ 23 de octubre de 2026])
]
#outline(title: [Índice], depth: 2, indent: auto)
#pagebreak()
```

# Cómo usar estos apuntes {.unnumbered}

Estos apuntes recogen todo el contenido de la presentación del curso, ordenado para leerlo con calma o imprimirlo. Cada apartado corresponde a una diapositiva y suele tener tres partes:

- **El código**, con el mismo número de paso que en clase (*Paso 1*, *Paso 2*…) y, debajo, la salida que devuelve R. El código es **acumulativo**: cada paso usa los objetos creados en los anteriores, así que conviene ejecutarlo en orden en un mismo script.
- **Las figuras y tablas**. Las que en clase eran interactivas aparecen aquí en una versión estática que resume lo que se veía al manipularlas.
- **En pocas palabras**: la explicación de qué significa el resultado y por qué importa.

Para reproducir los análisis necesitas R (o posit.cloud), los paquetes `tidyverse`, `rpart`, `rpart.plot`, `pheatmap` y `plotly`, y la carpeta `datos/` con `ele.csv`, `errores.csv` y `frecuencias.csv`. Los datos son **simulados** con fines docentes, aunque se comportan como podrían hacerlo unos datos reales de aprendientes de ELE. Si una salida tuya no coincide con la de estos apuntes, revisa el paso anterior.

Los apuntes siguen el orden del curso: cinco sesiones (descriptiva, visualización, inferencia, chi cuadrado y ANOVA, y árboles de decisión), un capítulo **Plus** con material complementario (estadística de corpus y modelos de regresión lineal y logística) y un cierre sobre cómo elegir el método e informar de los resultados.

La versión interactiva de todos los gráficos está en la presentación HTML del curso (`estadistica_ELE_interactiva.html`). Cuando la explicación habla de mover un deslizador o pulsar un botón, se refiere a esa versión.

'''


def main():
    src = ORIGEN.read_text(encoding="utf-8")
    cuerpo = src.split("\n---\n", 1)[1]
    trozos = re.split(r"\n(?=## )", cuerpo)[1:]      # el primero es el bloque de configuración
    est, partes = Estado(), [CABECERA]
    for t in trozos:
        cab, _, contenido = t.partition("\n")
        titulo_attrs = cab[3:].strip()
        if ".hero" in titulo_attrs:
            h = hero(titulo_attrs, contenido)
            if h:
                est.sesiones += 1
            partes.append(h); continue
        titulo = re.sub(r"\s*\{[^}]*\}\s*$", "", titulo_attrs).strip()
        if titulo in OMITIR:
            continue
        notas = []
        texto = render(analizar(contenido.splitlines()), est, notas)
        texto = re.sub(r"\n{3,}", "\n\n", texto).strip()
        sufijo = "" if est.sesiones else " {.unnumbered}"
        bloque = f"## {titulo}{sufijo}\n\n{texto}\n"
        if notas:
            bloque += '\n::: {.callout-note title="En pocas palabras" icon=false}\n' + "\n\n".join(notas) + "\n:::\n"
        partes.append(bloque + "\n")
    doc = re.sub(r"\[([^\]]+)\]\(#/[^)]+\)", r"\1", "\n".join(partes))   # enlaces internos de reveal
    DESTINO.write_text(doc, encoding="utf-8")
    print(f"Escrito {DESTINO.name}: {len(trozos)} diapositivas procesadas")


if __name__ == "__main__":
    main()
