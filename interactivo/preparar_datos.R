# Exporta los datos y los resultados de R que usan los componentes interactivos (D3).
# Se ejecuta en el bloque de configuración de la presentación y escribe interactivo/datos_curso.js.
# Todos los cálculos reproducen el código que copian los alumnos (mismas semillas, mismas llamadas).

library(jsonlite)

# Convierte un árbol de rpart en una lista de nodos (id, condición de entrada, n, predicción)
arbol_a_lista <- function(fit) {
  fr  <- fit$frame
  ids <- as.integer(rownames(fr))
  etq <- labels(fit, minlength = 0)
  clase <- fit$method == "class"
  lapply(seq_len(nrow(fr)), function(i) {
    nodo <- list(id = ids[i], cond = etq[i], n = fr$n[i],
                 hoja = fr$var[i] == "<leaf>", var = as.character(fr$var[i]), dev = fr$dev[i])
    if (clase) {
      k <- (ncol(fr$yval2) - 1) / 2
      nodo$clase   <- attr(fit, "ylevels")[fr$yval[i]]
      nodo$conteos <- unname(fr$yval2[i, 2:(1 + k)])
      nodo$p       <- unname(fr$yval2[i, 2 + k])        # P(primera clase) = P(correcto)
    } else {
      nodo$media <- fr$yval[i]
    }
    nodo
  })
}

cp_a_tabla <- function(fit) {
  as.data.frame(fit$cptable) |>
    setNames(c("cp", "nsplit", "rel_error", "xerror", "xstd"))
}

# --- Árboles del subjuntivo (mismo código que en las diapositivas) ---
set.seed(2026)
w_arbol_grande <- rpart(subjuntivo ~ horas + estancia + L1 + nivel,
                        data = ele, method = "class", cp = 0.01)
w_arbol_podado <- prune(w_arbol_grande, cp = 0.02)

set.seed(2026)
w_arbol_total <- rpart(subjuntivo ~ horas + estancia + L1 + nivel, data = ele,
                       method = "class", control = rpart.control(cp = 0, minsplit = 6))

w_arbol_nota <- rpart(nota_dele ~ horas + estancia + L1, data = ele, cp = 0.03)

# --- Entrenamiento / prueba ---
set.seed(2026)
w_idx <- sample(nrow(ele), size = 0.7 * nrow(ele))
w_entreno <- ele[w_idx, ]; w_prueba <- ele[-w_idx, ]
w_ae <- rpart(subjuntivo ~ horas + estancia + L1 + nivel, data = w_entreno,
              method = "class", control = rpart.control(cp = 0, minsplit = 6))
w_cp_opt <- w_ae$cptable[which.min(w_ae$cptable[, "xerror"]), "CP"]
w_ap <- prune(w_ae, cp = w_cp_opt)
w_a1 <- prune(w_ae, cp = w_ae$cptable[2, "CP"])          # un solo corte
matriz <- function(m, d) {
  t <- table(real = d$subjuntivo, pred = predict(m, d, type = "class"))
  list(vp = t["correcto", "correcto"], fn = t["correcto", "incorrecto"],
       fp = t["incorrecto", "correcto"], vn = t["incorrecto", "incorrecto"])
}
w_holdout <- list(
  grande = list(hojas = sum(w_ae$frame$var == "<leaf>"),
                entreno = matriz(w_ae, w_entreno), prueba = matriz(w_ae, w_prueba)),
  podado = list(hojas = sum(w_ap$frame$var == "<leaf>"),
                entreno = matriz(w_ap, w_entreno), prueba = matriz(w_ap, w_prueba)),
  simple = list(hojas = sum(w_a1$frame$var == "<leaf>"),
                entreno = matriz(w_a1, w_entreno), prueba = matriz(w_a1, w_prueba)))

# --- Otros resultados ---
w_tab <- table(ele$estancia, ele$subjuntivo)
w_chi <- suppressWarnings(chisq.test(w_tab))
w_aov <- aov(nota_dele ~ L1, data = ele)
w_tuk <- TukeyHSD(w_aov)$L1
set.seed(6); w_sub <- sample(nrow(ele), 40)
w_ols <- coef(lm(nota_dele ~ horas, data = ele[w_sub, ]))
w_glm1 <- coef(glm(subjuntivo == "correcto" ~ horas, data = ele, family = binomial))
w_ttest <- t.test(nota_dele ~ estancia, data = ele)

CURSO <- list(
  ele = ele,
  errores = errores,
  frec = frec, N = list(aprend = 150000, nativo = 200000),
  anscombe = anscombe |>
    tidyr::pivot_longer(everything(), names_to = c(".value", "set"), names_pattern = "(.)(.)"),
  chi = list(filas = rownames(w_tab), cols = colnames(w_tab), O = unclass(w_tab),
             stdres = round(unclass(w_chi$stdres), 2), x2 = unname(w_chi$statistic), p = w_chi$p.value),
  tukey = data.frame(comp = rownames(w_tuk), w_tuk, row.names = NULL) |>
    setNames(c("comp", "diff", "lwr", "upr", "p")),
  interaccion = ele |> dplyr::group_by(L1, estancia) |> dplyr::summarise(m = mean(nota_dele), .groups = "drop"),
  ols = list(ids = w_sub, b0 = unname(w_ols[1]), b1 = unname(w_ols[2])),
  glm1 = list(b0 = unname(w_glm1[1]), b1 = unname(w_glm1[2])),
  ttest = list(t = unname(w_ttest$statistic), gl = unname(w_ttest$parameter), p = w_ttest$p.value),
  arboles = list(grande = arbol_a_lista(w_arbol_grande), podado = arbol_a_lista(w_arbol_podado),
                 nota = arbol_a_lista(w_arbol_nota)),
  cptabla = cp_a_tabla(w_arbol_total),
  importancia = data.frame(var = names(w_arbol_grande$variable.importance),
                           imp = unname(w_arbol_grande$variable.importance)),
  holdout = w_holdout
)

writeLines(paste0("window.CURSO = ", toJSON(CURSO, auto_unbox = TRUE, digits = 6, dataframe = "rows",
                                            matrix = "rowmajor"), ";"),
           "interactivo/datos_curso.js")
rm(list = ls(pattern = "^w_"))
