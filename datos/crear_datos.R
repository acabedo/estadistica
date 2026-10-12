# Genera los datos simulados del curso (aprendientes de ELE)
set.seed(2026)
n <- 240
L1    <- sample(c("inglés", "italiano", "chino", "árabe"), n, replace = TRUE)
nivel <- sample(c("A2", "B1", "B2", "C1"), n, replace = TRUE, prob = c(.2, .3, .3, .2))
estancia <- sample(c("sí", "no"), n, replace = TRUE, prob = c(.4, .6))
horas <- round(pmax(20, rnorm(n, 180, 60) + 60 * (match(nivel, c("A2","B1","B2","C1")) - 1)))
ef_L1 <- c(inglés = 0, italiano = 6, chino = -7, árabe = -4)[L1]
nota  <- round(pmin(100, pmax(20, 45 + 0.08 * horas + ef_L1 + 5 * (estancia == "sí") + rnorm(n, 0, 9))))
lam   <- exp(2.2 - 0.004 * horas - 0.3 * (L1 == "italiano") + 0.25 * (L1 == "chino"))
err_ser_estar <- rpois(n, lam)
p_subj <- plogis(-4 + 0.012 * horas + 0.8 * (estancia == "sí") + 0.6 * (L1 == "italiano"))
subjuntivo <- ifelse(runif(n) < p_subj, "correcto", "incorrecto")
ele <- data.frame(id = sprintf("A%03d", 1:n), L1, nivel, estancia, horas,
                  nota_dele = nota, err_ser_estar, subjuntivo)
write.csv(ele, "datos/ele.csv", row.names = FALSE)

# Tipos de error por L1 (recuentos en un corpus de aprendices)
errores <- data.frame(
  L1 = rep(c("inglés", "italiano", "chino", "árabe"), each = 6),
  tipo = rep(c("ser/estar", "subjuntivo", "artículos", "preposiciones", "concordancia", "léxico"), 4),
  n = c(95, 80, 20, 60, 25, 40,   45, 50, 15, 70, 20, 55,
        70, 90, 110, 65, 80, 60,  60, 75, 45, 85, 70, 50))
write.csv(errores, "datos/errores.csv", row.names = FALSE)

# Frecuencias: corpus de aprendices vs corpus nativo (palabras por corpus)
frec <- data.frame(
  palabra = c("muy", "cosa", "bueno", "pues", "o sea", "es que", "además", "sin embargo", "hacer", "tener"),
  aprend = c(620, 410, 380, 90, 25, 60, 140, 70, 520, 480),
  nativo = c(410, 250, 300, 380, 190, 210, 150, 60, 450, 470))
attr(frec, "N") <- c(aprend = 150000, nativo = 200000)
write.csv(frec, "datos/frecuencias.csv", row.names = FALSE)
