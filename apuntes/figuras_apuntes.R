# Figuras y tablas estáticas de los apuntes: versiones imprimibles de los componentes
# interactivos de la presentación. Cada función devuelve un gráfico (ggplot) o una tabla (kable).

library(patchwork)
library(ggrepel)

col <- list(azul = "#1D3557", rojo = "#E63946", verde = "#2A9D8F", arena = "#F4A261",
            celeste = "#457B9D", agua = "#A8DADC")
pal_L1 <- c("árabe" = col$azul, "chino" = col$rojo, "inglés" = col$verde, "italiano" = col$arena)
tabla <- function(d, ...) knitr::kable(d, format.args = list(decimal.mark = ","), ...)
gini <- function(a, b) { n <- a + b; if (n == 0) return(0); p <- a / n; 1 - p^2 - (1 - p)^2 }

# ---------------------------------------------------------------- Sesión 1

fig_muestra <- function() {
  set.seed(1)
  sim <- map_dfr(c(5, 30, 120), \(k) tibble(n = paste("Muestras de n =", k),
                 m = replicate(1000, mean(sample(ele$nota_dele, k)))))
  ggplot(sim, aes(m)) +
    geom_histogram(binwidth = .5, fill = col$azul, colour = "white") +
    geom_vline(xintercept = mean(ele$nota_dele), colour = col$rojo, linewidth = 1) +
    facet_wrap(~ fct_inorder(n), nrow = 1) +
    labs(x = "Media de cada muestra (1000 muestras; en rojo, la media real)", y = "Número de muestras")
}

fig_media_mediana <- function() {
  tibble(x = 10:120) |>
    mutate(media = map_dbl(x, \(v) mean(c(12, 14, 13, 15, 14, v))),
           mediana = map_dbl(x, \(v) median(c(12, 14, 13, 15, 14, v)))) |>
    pivot_longer(-x) |>
    ggplot(aes(x, value, colour = name)) + geom_line(linewidth = 1.3) +
    scale_colour_manual(values = c(media = col$rojo, mediana = col$verde), name = NULL) +
    labs(x = "Tiempo del sexto estudiante (los otros cinco: 12, 13, 14, 14, 15 s)",
         y = "Media y mediana (s)") +
    theme(legend.position = "top")
}

fig_dispersion <- function() {
  tibble(x = seq(20, 120, .5)) |>
    mutate(`Grupo A (DE = 4)` = dnorm(x, 70, 4), `Grupo B (DE = 15)` = dnorm(x, 70, 15)) |>
    pivot_longer(-x) |>
    ggplot(aes(x, value, fill = name, colour = name)) +
    geom_area(alpha = .45, position = "identity") +
    geom_vline(xintercept = 70, linetype = 2) +
    scale_fill_manual(values = c(col$verde, col$rojo), name = NULL) +
    scale_colour_manual(values = c(col$verde, col$rojo), name = NULL) +
    labs(x = "Nota (las dos curvas tienen media 70)", y = "Densidad") +
    theme(legend.position = "top")
}

tabla_de_pasos <- function() {
  x <- c(55, 62, 70, 71, 92)
  tibble(`Nota` = as.character(x), `① Distancia a la media (x − 70)` = sprintf("%+d", x - 70),
         `② Al cuadrado` = as.character((x - 70)^2)) |>
    add_row(`Nota` = "**Suma**", `① Distancia a la media (x − 70)` = "0",
            `② Al cuadrado` = "**774**") |>
    add_row(`Nota` = "**Varianza**", `① Distancia a la media (x − 70)` = "",
            `② Al cuadrado` = "774 / 4 = **193,5**") |>
    add_row(`Nota` = "**DE**", `① Distancia a la media (x − 70)` = "",
            `② Al cuadrado` = "√193,5 ≈ **13,9**") |>
    tabla(align = "rrr")
}

fig_de_pasos <- function() {
  d <- tibble(i = 1:5, x = c(55, 62, 70, 71, 92)); de <- sd(d$x)
  ggplot(d, aes(i, x)) +
    annotate("rect", xmin = .6, xmax = 5.4, ymin = 70 - de, ymax = 70 + de, fill = col$agua, alpha = .4) +
    geom_hline(yintercept = 70, colour = col$rojo, linewidth = 1.1) +
    geom_segment(aes(xend = i, yend = 70), linetype = 2, colour = col$celeste) +
    geom_point(size = 3.5, colour = col$azul) +
    annotate("text", x = 5.45, y = 72, label = "media = 70", colour = col$rojo, hjust = 1, size = 3.3) +
    annotate("text", x = .65, y = 70 + de + 2, label = "± 1 DE = 13,9", hjust = 0, size = 3.3, colour = col$celeste) +
    labs(x = "Estudiante", y = "Nota")
}

fig_cuartiles <- function() {
  qs <- quantile(ele$nota_dele)
  ggplot(ele, aes(nota_dele)) +
    geom_dotplot(binwidth = 1, dotsize = .6, fill = col$agua, colour = NA) +
    geom_vline(xintercept = qs[2:4], colour = c(col$celeste, col$rojo, col$celeste), linewidth = 1) +
    annotate("label", x = qs[2:4], y = .95, label = c("Q1", "Mediana", "Q3"), size = 3.3) +
    theme(axis.text.y = element_blank()) + labs(y = NULL, x = "Nota DELE (cada punto, un estudiante)")
}

fig_resumen_grupos <- function() {
  ele |>
    pivot_longer(c(L1, nivel, estancia), names_to = "Agrupar por", values_to = "grupo",
                 values_transform = as.character) |>
    group_by(`Agrupar por`, grupo) |>
    summarise(m = mean(nota_dele), s = sd(nota_dele), .groups = "drop") |>
    ggplot(aes(grupo, m)) +
    geom_pointrange(aes(ymin = m - s, ymax = m + s), colour = col$azul) +
    geom_text(aes(label = format(round(m, 1), decimal.mark = ",")), nudge_x = .32, size = 3) +
    facet_wrap(~ `Agrupar por`, scales = "free_x") +
    labs(x = NULL, y = "Nota DELE (media ± DE)")
}

# ---------------------------------------------------------------- Sesión 2

fig_anscombe <- function() {
  anscombe |>
    pivot_longer(everything(), names_to = c(".value", "set"), names_pattern = "(.)(.)") |>
    mutate(set = paste("Conjunto", c("I", "II", "III", "IV")[as.integer(set)])) |>
    ggplot(aes(x, y)) + geom_point(colour = col$azul, size = 2) +
    geom_smooth(method = "lm", se = FALSE, colour = col$rojo, linewidth = .9) +
    facet_wrap(~ set, nrow = 1) +
    labs(subtitle = "Los cuatro: media de x = 9, media de y = 7,5, r = 0,82 y la misma recta y = 3 + 0,5x")
}

fig_histograma <- function() {
  h <- function(bw) ggplot(ele, aes(nota_dele)) +
    geom_histogram(binwidth = bw, fill = col$azul, colour = "white") +
    labs(title = paste("binwidth =", bw), x = "Nota", y = NULL)
  h(1) + h(5) + h(20)
}

fig_geoms <- function() {
  ggplot(ele, aes(nivel, nota_dele)) +
    geom_violin(fill = col$agua, colour = NA, alpha = .7) +
    geom_boxplot(width = .2, fill = "white", outlier.shape = NA) +
    geom_jitter(width = .08, alpha = .35, colour = col$azul, size = 1) +
    labs(x = "Nivel", y = "Nota DELE", subtitle = "Violín (forma) + caja (resumen) + puntos (cada persona)")
}

fig_motion <- function() {
  resumen <- ele |> group_by(nivel, L1) |>
    summarise(horas = mean(horas), nota = mean(nota_dele), n = n(), .groups = "drop")
  ggplot(resumen, aes(horas, nota, colour = L1)) +
    geom_path(aes(group = L1), arrow = arrow(length = unit(.22, "cm"), type = "closed"), linewidth = .9) +
    geom_point(aes(size = n)) +
    geom_text(aes(label = nivel), vjust = -1.2, size = 2.8, show.legend = FALSE) +
    scale_colour_manual(values = pal_L1) + scale_size(range = c(2, 6)) +
    labs(x = "Horas medias", y = "Nota media", size = "n",
         subtitle = "Versión impresa del motion chart: la flecha recorre los niveles de A2 a C1")
}

fig_heatmap <- function() {
  m <- xtabs(n ~ L1 + tipo, errores)
  as.data.frame(as.table(suppressWarnings(chisq.test(m))$residuals)) |>
    ggplot(aes(tipo, L1, fill = Freq)) + geom_tile(colour = "white") +
    geom_text(aes(label = format(round(Freq, 1), decimal.mark = ",")), size = 3.3) +
    scale_fill_gradient2(low = col$rojo, mid = "white", high = col$verde, name = "Residuo") +
    labs(x = NULL, y = NULL, subtitle = "Residuos: verde = más errores de los esperados; rojo = menos")
}

tabla_elige_grafico <- function() {
  tribble(
    ~`Variable 1`, ~`Variable 2`, ~`Gráfico`, ~`Código de ggplot2`,
    "Numérica", "—", "Histograma o densidad", "`geom_histogram()`, `geom_density()`",
    "Categórica", "—", "Barras", "`geom_bar()`",
    "Numérica", "Categórica", "Boxplot o violín (+ puntos)", "`geom_boxplot()`, `geom_violin()`, `geom_jitter()`",
    "Numérica", "Numérica", "Dispersión", "`geom_point()` + `geom_smooth()`",
    "Categórica", "Categórica", "Barras apiladas al 100 % o mosaico", "`geom_bar(position = \"fill\")`",
    "Categórica", "Categórica con muchas celdas", "Mapa de calor", "`geom_tile()`") |>
    tabla()
}

# ---------------------------------------------------------------- Sesión 3

fig_normal <- function() {
  tibble(x = seq(-4, 4, .01), y = dnorm(x)) |>
    ggplot(aes(x, y)) +
    geom_area(data = \(d) filter(d, abs(x) <= 1.96), fill = col$agua) +
    geom_line(linewidth = 1, colour = col$azul) +
    annotate("text", x = 0, y = .15, label = "95 %", size = 6, colour = col$azul) +
    scale_x_continuous(breaks = -3:3, labels = c("−3 DE", "−2 DE", "−1 DE", "media", "+1 DE", "+2 DE", "+3 DE")) +
    labs(x = NULL, y = NULL) + theme(axis.text.y = element_blank())
}

fig_muestral <- function() {
  set.seed(10); s <- sd(ele$nota_dele); mu <- mean(ele$nota_dele)
  sim <- map_dfr(c(5, 30, 100), \(k) tibble(
    n = sprintf("n = %d · EE = σ/√n = %s", k, format(round(s / sqrt(k), 2), decimal.mark = ",")),
    m = replicate(2000, mean(sample(ele$nota_dele, k, replace = TRUE)))))
  ggplot(sim, aes(m)) +
    geom_histogram(binwidth = .5, fill = col$celeste, colour = "white") +
    geom_vline(xintercept = mu, colour = col$rojo, linewidth = 1) +
    facet_wrap(~ fct_inorder(n), nrow = 1) +
    labs(x = "Media de cada muestra (2000 muestras)", y = "Frecuencia")
}

fig_ic <- function() {
  set.seed(4); mu <- mean(ele$nota_dele)
  map_dfr(1:50, \(i) { x <- sample(ele$nota_dele, 30); ee <- sd(x) / sqrt(30)
    tibble(i, m = mean(x), inf = m - qt(.975, 29) * ee, sup = m + qt(.975, 29) * ee) }) |>
    mutate(ok = inf < mu & sup > mu) |>
    ggplot(aes(i, m, colour = ok)) + geom_hline(yintercept = mu, linetype = 2) +
    geom_pointrange(aes(ymin = inf, ymax = sup), size = .2) +
    scale_colour_manual(values = c(`TRUE` = col$celeste, `FALSE` = col$rojo),
                        labels = c(`TRUE` = "Contiene la media real", `FALSE` = "No la contiene"), name = NULL) +
    labs(x = "50 muestras de 30 estudiantes", y = "IC 95 % de la media") + theme(legend.position = "top")
}

fig_valor_p <- function() {
  t_obs <- 2.2
  tibble(x = seq(-4, 4, .01), y = dt(x, 100)) |>
    ggplot(aes(x, y)) + geom_line(linewidth = 1, colour = col$azul) +
    geom_area(data = \(d) filter(d, x >= t_obs), fill = col$rojo, alpha = .75) +
    geom_area(data = \(d) filter(d, x <= -t_obs), fill = col$rojo, alpha = .75) +
    geom_vline(xintercept = qt(c(.025, .975), 100), colour = col$arena, linewidth = .9) +
    geom_vline(xintercept = t_obs, linetype = 2) +
    annotate("text", x = t_obs + .1, y = .3, label = "t observado = 2,2", hjust = 0, size = 3.3) +
    annotate("text", x = 3.1, y = .07, label = "p = área roja\n= 0,03", colour = col$rojo, size = 3.3) +
    labs(x = "Valores de t si H0 fuera cierta (en naranja, los valores críticos ±1,98)", y = NULL) +
    theme(axis.text.y = element_blank())
}

fig_potencia <- function() {
  zc <- qnorm(.975); delta <- .5 * sqrt(30 / 2)
  d1 <- tibble(z = seq(-4, 8, .01), h0 = dnorm(z), h1 = dnorm(z, delta))
  p1 <- ggplot(d1, aes(z)) +
    geom_area(data = filter(d1, z <= zc), aes(y = h1), fill = col$arena, alpha = .6) +
    geom_area(data = filter(d1, z >= zc), aes(y = h1), fill = col$verde, alpha = .45) +
    geom_area(data = filter(d1, z >= zc), aes(y = h0), fill = col$rojo, alpha = .85) +
    geom_line(aes(y = h0), colour = col$azul) + geom_line(aes(y = h1), colour = col$verde) +
    geom_vline(xintercept = zc, linetype = 2) +
    annotate("text", x = c(-1.3, delta + 1.4), y = .43, label = c("H0 cierta", "H1 cierta"), size = 3.2, colour = c(col$azul, col$verde)) +
    annotate("text", x = c(1, 3.4, 2.6), y = c(.06, .2, .015), label = c("β", "potencia", "α"), size = 3.4) +
    labs(title = "d = 0,5 · n = 30 por grupo", x = "Estadístico z", y = NULL) +
    theme(axis.text.y = element_blank())
  pot <- expand_grid(d = c(.2, .5, .8), n = 5:200) |>
    mutate(p = 1 - pnorm(zc - d * sqrt(n / 2)) + pnorm(-zc - d * sqrt(n / 2)), d = factor(format(d, decimal.mark = ",")))
  p2 <- ggplot(pot, aes(n, p, colour = d)) + geom_line(linewidth = 1) +
    geom_hline(yintercept = .8, linetype = 2) +
    scale_y_continuous(labels = scales::percent) +
    scale_colour_manual(values = c(col$rojo, col$arena, col$verde), name = "d") +
    labs(title = "Potencia según n y d", x = "n por grupo", y = "Potencia")
  p1 + p2
}

fig_cohen_d <- function() {
  expand_grid(d = c(.41, .8, 1.5), x = seq(-4, 5.5, .02)) |>
    mutate(g1 = dnorm(x), g2 = dnorm(x, d), x = 65.9 + 12.7 * x,
           d = sprintf("d = %s: %s puntos (solapamiento %s %%)", format(d, decimal.mark = ","),
                       format(round(12.7 * d, 1), decimal.mark = ","), round(100 * 2 * pnorm(-d / 2)))) |>
    ggplot(aes(x)) +
    geom_area(aes(y = g1), fill = col$azul, alpha = .35) + geom_area(aes(y = g2), fill = col$rojo, alpha = .35) +
    facet_wrap(~ fct_inorder(d), nrow = 1) +
    labs(x = "Nota DELE (azul: sin estancia; rojo: con estancia, desplazada d desviaciones típicas)", y = NULL) +
    theme(axis.text.y = element_blank())
}

tabla_elige_prueba <- function() {
  tribble(
    ~Respuesta, ~`Situación`, ~Prueba, ~`En R`, ~Efecto,
    "Numérica", "2 grupos distintos, datos normales (o n > 30)", "t de Welch", "`t.test(y ~ g)`", "d de Cohen",
    "Numérica", "2 grupos distintos, no normales", "Wilcoxon (Mann-Whitney)", "`wilcox.test(y ~ g)`", "r",
    "Numérica", "Mismas personas dos veces, normal", "t para muestras relacionadas", "`t.test(post, pre, paired = TRUE)`", "d",
    "Numérica", "Mismas personas dos veces, no normal", "Wilcoxon de rangos con signo", "`wilcox.test(post, pre, paired = TRUE)`", "r",
    "Numérica", "3 o más grupos, normal y varianzas parecidas", "ANOVA + Tukey", "`aov()`, `TukeyHSD()`", "η²",
    "Numérica", "3 o más grupos, no normal", "Kruskal-Wallis", "`kruskal.test()`", "ε²",
    "Numérica", "Relación con otra numérica, lineal y normal", "Correlación de Pearson", "`cor.test(x, y)`", "r, r²",
    "Numérica", "Relación con otra numérica, ordinal o con extremos", "Correlación de Spearman", "`cor.test(x, y, method = \"spearman\")`", "ρ",
    "Numérica", "Predecir a partir de uno o varios predictores", "Regresión lineal (o árbol de regresión)", "`lm()`", "R²",
    "Categórica", "Asociación entre dos categóricas, esperadas ≥ 5", "Chi cuadrado", "`chisq.test()`", "V de Cramér",
    "Categórica", "Asociación con celdas pequeñas", "Prueba exacta de Fisher", "`fisher.test()`", "odds ratio",
    "Categórica", "Predecir con varios predictores y cuantificar efectos", "Regresión logística", "`glm(family = binomial)`", "odds ratios",
    "Categórica", "Predecir con reglas fáciles de explicar", "Árbol de decisión", "`rpart()`", "acierto",
    "Frecuencias", "Una palabra en dos corpus", "Log-likelihood + log-ratio", "`ll()`, `log2()`", "log-ratio") |>
    mutate(`Variable respuesta y situación` = paste0("**", Respuesta, "**: ", `Situación`), .before = 1) |>
    select(-Respuesta, -`Situación`) |>
    tabla()
}

fig_senal_ruido <- function() {
  set.seed(8)
  bind_rows(tibble(caso = "Poco ruido → t grande", g = rep(c("A", "B"), each = 40), v = c(rnorm(40, 65, 4), rnorm(40, 72, 4))),
            tibble(caso = "Mucho ruido → t pequeño", g = rep(c("A", "B"), each = 40), v = c(rnorm(40, 65, 16), rnorm(40, 72, 16)))) |>
    ggplot(aes(g, v, colour = g)) + geom_jitter(width = .12, alpha = .6, show.legend = FALSE) +
    stat_summary(fun = mean, geom = "crossbar", width = .5, colour = "black") +
    facet_wrap(~ fct_rev(caso)) + scale_colour_manual(values = c(col$azul, col$rojo)) +
    labs(x = "Grupo", y = "Nota", subtitle = "La misma diferencia de medias (7 puntos) con distinta variabilidad")
}

tabla_wilcoxon <- function() {
  a <- c(3, 5, 2, 6, 4, 8, 3, 12); b <- c(1, 2, 0, 3, 1, 4, 2, 1)
  r <- rank(c(a, b)); fr <- \(x) format(x, decimal.mark = ",", trim = TRUE)
  tibble(Grupo = c("Sin estancia", "Con estancia"),
         `Errores de ser/estar` = c(paste(a, collapse = ", "), paste(b, collapse = ", ")),
         `Rangos (todos juntos, de menor a mayor)` = c(paste(fr(r[1:8]), collapse = ", "), paste(fr(r[9:16]), collapse = ", ")),
         `Suma de rangos` = c(fr(sum(r[1:8])), fr(sum(r[9:16])))) |>
    tabla()
}

fig_correlacion <- function() {
  set.seed(5)
  map_dfr(c(-0.8, 0, 0.5, 0.9), \(r) { x <- rnorm(150)
    tibble(etq = paste("r =", format(r, decimal.mark = ",")), x = x, y = r * x + sqrt(1 - r^2) * rnorm(150)) }) |>
    ggplot(aes(x, y)) + geom_point(alpha = .5, colour = col$azul, size = 1) +
    facet_wrap(~ fct_inorder(etq), nrow = 1) +
    theme(axis.text = element_blank()) + labs(x = NULL, y = NULL)
}

# ---------------------------------------------------------------- Sesión 4

fig_chi <- function() {
  t <- table(estancia = ele$estancia, subjuntivo = ele$subjuntivo)
  E <- outer(rowSums(t), colSums(t)) / sum(t)
  bind_rows(as_tibble(as.data.frame(t)) |> mutate(tipo = "Observadas"),
            as_tibble(as.data.frame(as.table(E))) |> rename(estancia = Var1, subjuntivo = Var2) |> mutate(tipo = "Esperadas si no hubiera relación")) |>
    mutate(tipo = fct_inorder(tipo), estancia = paste("estancia:", estancia)) |>
    ggplot(aes(estancia, Freq, fill = subjuntivo)) +
    geom_col(position = "fill", width = .7) +
    geom_text(aes(label = format(round(Freq, 1), decimal.mark = ",")), position = position_fill(vjust = .5), colour = "white", size = 3.2) +
    facet_wrap(~ tipo) + scale_fill_manual(values = c(col$verde, col$rojo)) +
    scale_y_continuous(labels = scales::percent) + labs(x = NULL, y = "Proporción")
}

fig_fwer <- function() {
  tibble(k = 1:20, `Sin corregir` = 1 - .95^k, Bonferroni = 1 - (1 - .05 / k)^k) |>
    pivot_longer(-k) |>
    ggplot(aes(k, value, colour = name)) + geom_line(linewidth = 1) + geom_point(size = 1.6) +
    geom_hline(yintercept = .05, linetype = 2) +
    annotate("text", x = 6.4, y = 1 - .95^6, label = "4 grupos = 6 pruebas → 26 %", hjust = 0, size = 3.2) +
    scale_colour_manual(values = c(Bonferroni = col$verde, `Sin corregir` = col$rojo), name = NULL) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Número de comparaciones", y = "P(al menos un falso positivo)") + theme(legend.position = "top")
}

fig_anova_f <- function() {
  set.seed(40)
  esc <- function(etq, sep, s) tibble(caso = etq, L1 = rep(names(pal_L1), each = 25),
                                      v = 68 + rep(c(-1.5, .5, 1.5, -.5) * sep, each = 25) + rnorm(100, 0, s))
  d <- bind_rows(esc("Medias separadas, poco ruido → F grande", 6, 6), esc("Medias juntas, mucho ruido → F ≈ 1", 1, 15))
  ggplot(d, aes(L1, v, colour = L1)) + geom_jitter(width = .15, alpha = .55, show.legend = FALSE, size = 1) +
    stat_summary(fun = mean, geom = "crossbar", width = .5, colour = "black") +
    geom_hline(data = \(x) summarise(x, m = mean(v), .by = caso), aes(yintercept = m), linetype = 2) +
    facet_wrap(~ caso) + scale_colour_manual(values = pal_L1) + labs(x = NULL, y = "Nota")
}

fig_tukey <- function() {
  as.data.frame(TukeyHSD(modelo_aov)$L1) |> rownames_to_column("comp") |>
    mutate(sig = `p adj` < .05, comp = fct_reorder(comp, diff)) |>
    ggplot(aes(diff, comp, colour = sig)) + geom_vline(xintercept = 0, linetype = 2) +
    geom_pointrange(aes(xmin = lwr, xmax = upr)) +
    scale_colour_manual(values = c(`TRUE` = col$rojo, `FALSE` = "grey60"),
                        labels = c(`TRUE` = "Significativa", `FALSE` = "No significativa"), name = NULL) +
    labs(x = "Diferencia de medias (IC 95 % ajustado)", y = NULL) + theme(legend.position = "top")
}

fig_interaccion <- function() {
  real <- ele |> group_by(L1, estancia) |> summarise(m = mean(nota_dele), .groups = "drop") |>
    mutate(caso = "Nuestros datos")
  ficticio <- function(caso, no, si) tibble(caso, L1 = rep(names(pal_L1), 2), estancia = rep(c("no", "sí"), each = 4), m = c(no, si))
  bind_rows(real,
            ficticio("Sin interacción (ficticio)", c(60, 55, 68, 72), c(66, 61, 74, 78)),
            ficticio("Ordinal (ficticio)", c(60, 55, 66, 70), c(63, 57, 78, 86)),
            ficticio("Cruzada (ficticio)", c(70, 55, 66, 78), c(58, 68, 78, 62))) |>
    mutate(caso = fct_inorder(caso)) |>
    ggplot(aes(estancia, m, colour = L1, group = L1)) + geom_line(linewidth = 1) + geom_point(size = 2) +
    facet_wrap(~ caso, nrow = 1) + scale_colour_manual(values = pal_L1) +
    labs(x = "Estancia", y = "Nota media") + theme(legend.position = "top")
}

# ---------------------------------------------------------------- Sesión 5

fig_keyness <- function() {
  ggplot(frec, aes(LR, LL, colour = LR > 0)) +
    geom_hline(yintercept = 15.13, linetype = 2, colour = "grey50") +
    geom_vline(xintercept = c(-1, 1), linetype = 3, colour = "grey50") +
    geom_point(size = 2.8, show.legend = FALSE) +
    geom_text_repel(aes(label = palabra), size = 3.4, show.legend = FALSE, seed = 1) +
    scale_colour_manual(values = c(col$rojo, col$azul)) +
    labs(x = "Log-ratio: tamaño del efecto (− infrauso · + sobreuso)", y = "Log-likelihood: fiabilidad")
}

fig_simple_maths <- function() {
  frec |>
    mutate(`n = 1` = (pmw_aprend + 1) / (pmw_nativo + 1), `n = 1000` = (pmw_aprend + 1000) / (pmw_nativo + 1000)) |>
    pivot_longer(c(`n = 1`, `n = 1000`)) |>
    mutate(palabra = fct_reorder(palabra, value, .fun = max)) |>
    ggplot(aes(log2(value), palabra, fill = name)) + geom_col(position = "dodge", width = .7) +
    geom_vline(xintercept = 0) +
    scale_fill_manual(values = c(col$azul, col$arena), name = NULL) +
    labs(x = "log₂(Simple Maths): + sobreuso · − infrauso", y = NULL) + theme(legend.position = "top")
}

fig_regresion <- function() {
  set.seed(6); m <- ele[sample(nrow(ele), 40), ]
  fit <- lm(nota_dele ~ horas, m)
  ggplot(m, aes(horas, nota_dele)) +
    geom_segment(aes(xend = horas, yend = fitted(fit)), colour = col$rojo, alpha = .7) +
    geom_smooth(method = "lm", se = FALSE, colour = col$azul, linewidth = 1.1) +
    geom_point(size = 2.3, colour = col$azul) +
    labs(x = "Horas de instrucción", y = "Nota DELE",
         subtitle = "La recta de mínimos cuadrados hace mínima la suma de los residuos (en rojo) al cuadrado")
}

fig_logistica <- function() {
  g <- glm(subjuntivo == "correcto" ~ horas, data = ele, family = binomial)
  tibble(horas = 0:520) |> mutate(p = predict(g, tibble(horas), type = "response")) |>
    ggplot(aes(horas, p)) +
    geom_jitter(data = ele, aes(horas, as.integer(subjuntivo == "correcto")), height = .04, alpha = .25, colour = col$azul, size = 1) +
    geom_line(colour = col$rojo, linewidth = 1.2) +
    geom_point(data = ele |> mutate(t = cut(horas, seq(0, 550, 50))) |> group_by(t) |>
                 filter(n() >= 5) |> summarise(horas = mean(horas), p = mean(subjuntivo == "correcto")),
               shape = 18, size = 4, colour = col$arena) +
    labs(x = "Horas de instrucción (rombos: proporción real de aciertos por tramos de 50 horas)", y = "P(subjuntivo correcto)")
}

tabla_logistica <- function() {
  g <- glm(subjuntivo == "correcto" ~ horas, data = ele, family = binomial)
  tibble(horas = c(100, 250, 400)) |>
    mutate(`log-odds` = predict(g, tibble(horas)), odds = exp(`log-odds`), probabilidad = plogis(`log-odds`)) |>
    mutate(across(-horas, \(x) round(x, 2))) |> tabla()
}

# ---------------------------------------------------------------- Árboles

fig_anatomia_arbol <- function() {
  n <- tribble(~x, ~y, ~lab, ~fill,
               5, 4, "¿horas ≥ 243,5?", col$azul,
               2.6, 2.5, "¿nivel = C1?", col$celeste,
               7.6, 2.5, "incorrecto\n81 % · n = 79", col$rojo,
               1.2, 1, "correcto\n82 % · n = 62", col$verde,
               4, 1, "correcto\n59 % · n = 99", "#7fbfb3")
  e <- tibble(x = c(5, 5, 2.6, 2.6), y = c(4, 4, 2.5, 2.5), xe = c(2.6, 7.6, 1.2, 4), ye = c(2.5, 2.5, 1, 1),
              t = c("sí", "no", "sí", "no"))
  ggplot() +
    geom_segment(data = e, aes(x, y, xend = xe, yend = ye), colour = "grey55", linewidth = 1) +
    geom_label(data = e, aes((x + xe) / 2, (y + ye) / 2, label = t), size = 3, label.size = 0) +
    geom_label(data = n, aes(x, y, label = lab, fill = fill), colour = "white", fontface = "bold", size = 3.4,
               label.padding = unit(.5, "lines")) +
    annotate("text", x = c(5, 3.6, 0.7, 2.6), y = c(4.55, 3.4, 2.5, .35),
             label = c("RAÍZ", "RAMA", "NODO\nINTERNO", "HOJAS (nodos terminales)"),
             colour = col$rojo, fontface = "bold", size = 3.2) +
    scale_fill_identity() + xlim(0, 9) + ylim(.2, 4.7) + theme_void()
}

fig_gini <- function() {
  tibble(p = seq(0, 1, .01)) |>
    mutate(Gini = 1 - p^2 - (1 - p)^2, `Entropía (÷ 2)` = ifelse(p %in% c(0, 1), 0, -(p * log2(p) + (1 - p) * log2(1 - p))) / 2) |>
    pivot_longer(-p) |>
    ggplot(aes(p, value, colour = name, linetype = name)) + geom_line(linewidth = 1.1) +
    annotate("point", x = .7, y = 1 - .7^2 - .3^2, colour = col$rojo, size = 3) +
    annotate("text", x = .72, y = .45, label = "14 correctos de 20:\nGini = 0,42", hjust = 0, size = 3.2) +
    scale_colour_manual(values = c(col$celeste, col$rojo), name = NULL) + scale_linetype_manual(values = c(2, 1), name = NULL) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = "Proporción de «correcto» en el nodo", y = "Impureza") + theme(legend.position = "top")
}

fig_mejor_corte <- function() {
  ok <- ele$subjuntivo == "correcto"; gp <- gini(sum(ok), sum(!ok))
  gan_idx <- function(izq) { a <- sum(ok[izq]); b <- sum(ok[!izq])
    gp - (sum(izq) * gini(a, sum(izq) - a) + sum(!izq) * gini(b, sum(!izq) - b)) / length(ok) }
  hs <- sort(unique(ele$horas)); cortes <- (hs[-1] + hs[-length(hs)]) / 2
  curva <- tibble(c = cortes, g = map_dbl(cortes, \(c) gan_idx(ele$horas < c)))
  mejor <- curva |> slice_max(g, n = 1)
  mejor_cat <- function(v) { x <- as.character(ele[[v]]); niv <- names(sort(tapply(ok, x, mean), decreasing = TRUE))
    max(map_dbl(seq_len(length(niv) - 1), \(i) gan_idx(x %in% niv[1:i]))) }
  barras <- tibble(var = c("horas", "nivel", "L1", "estancia"),
                   g = c(mejor$g, mejor_cat("nivel"), mejor_cat("L1"), mejor_cat("estancia")))
  p1 <- ggplot(curva, aes(c, g)) + geom_line(colour = col$azul) +
    geom_point(data = mejor, colour = col$rojo, size = 2.5) +
    annotate("text", x = mejor$c + 8, y = mejor$g, label = paste("máximo:", format(mejor$c, decimal.mark = ",")), hjust = 0, colour = col$rojo, size = 3.2) +
    labs(title = "Cada corte posible de «horas»", x = "Punto de corte (horas)", y = "Reducción de Gini")
  p2 <- ggplot(barras, aes(g, fct_reorder(var, g), fill = var == "horas")) + geom_col(show.legend = FALSE) +
    scale_fill_manual(values = c("grey65", col$rojo)) +
    labs(title = "Mejor corte por variable", x = "Reducción de Gini", y = NULL)
  p1 + p2 + plot_layout(widths = c(1.6, 1))
}

tabla_crecer <- function() {
  fr <- arbol$frame; prof <- floor(log2(as.integer(rownames(fr))))
  hoja <- fr$var == "<leaf>"
  map_dfr(0:max(prof), \(k) { f <- prof == k | (prof < k & hoja)
    tibble(Profundidad = k, Hojas = sum(f), `Acierto en estos datos` = paste0(format(round(100 * sum(fr$n[f] - fr$dev[f]) / fr$n[1], 1), decimal.mark = ","), " %")) }) |>
    tabla()
}

tabla_reglas <- function() {
  fr <- arbol$frame; hojas <- as.integer(rownames(fr))[fr$var == "<leaf>"]
  caminos <- path.rpart(arbol, hojas, print.it = FALSE)
  tibble(Regla = map_chr(caminos, \(p) paste(gsub(">=", "≥ ", gsub("< ?", "< ", p[-1])), collapse = " y ")),
         `Predicción` = as.character(attr(arbol, "ylevels")[fr$yval[fr$var == "<leaf>"]]),
         `% correcto` = round(100 * fr$yval2[fr$var == "<leaf>", 4]),
         n = fr$n[fr$var == "<leaf>"]) |>
    arrange(desc(`% correcto`)) |>
    mutate(Regla = gsub("\\.", ",", Regla), Regla = gsub(",(?=\\S)", ", ", Regla, perl = TRUE),
           Regla = gsub("(\\d), (\\d)", "\\1,\\2", Regla)) |>
    tabla()
}

fig_cp <- function() {
  acc <- function(e) 1 - e * 116 / 240
  as_tibble(arbol_total$cptable) |>
    mutate(hojas = nsplit + 1) |>
    ggplot(aes(hojas)) +
    geom_ribbon(aes(ymin = acc(xerror + xstd), ymax = acc(xerror - xstd)), fill = col$rojo, alpha = .15) +
    geom_line(aes(y = acc(`rel error`), colour = "Entrenamiento (datos conocidos)"), linewidth = 1) +
    geom_line(aes(y = acc(xerror), colour = "Validación cruzada (estudiantes nuevos)"), linewidth = 1) +
    geom_point(aes(y = acc(`rel error`)), colour = col$azul) + geom_point(aes(y = acc(xerror)), colour = col$rojo) +
    geom_hline(yintercept = with(as_tibble(arbol_total$cptable), acc(min(xerror) + xstd[which.min(xerror)])), linetype = 2) +
    scale_colour_manual(values = c(col$azul, col$rojo), name = NULL) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Tamaño del árbol (número de hojas)", y = "Acierto") + theme(legend.position = "top")
}

tabla_confusion <- function() {
  arbol_s <- prune(arbol_e, cp = arbol_e$cptable[2, "CP"])
  mods <- list(`Sin podar` = arbol_e, `Podado (mín. xerror)` = arbol_p, `Un solo corte` = arbol_s)
  imap_dfr(mods, \(m, nom) tibble(`Árbol` = nom, Hojas = sum(m$frame$var == "<leaf>"),
                                  `Acierto en entrenamiento` = paste0(round(100 * acierto(m, entreno)), " %"),
                                  `Acierto en prueba` = paste0(round(100 * acierto(m, prueba)), " %"))) |>
    tabla()
}

tabla_matriz <- function() {
  t <- table(Realidad = prueba$subjuntivo, `Predicción` = predict(arbol_e, prueba, type = "class"))
  tibble(Realidad = rownames(t), `Predice correcto` = as.integer(t[, 1]), `Predice incorrecto` = as.integer(t[, 2])) |>
    tabla()
}
