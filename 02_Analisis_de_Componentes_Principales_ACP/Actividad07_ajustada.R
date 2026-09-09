################################################################################
# ACTIVIDAD 07: Análisis de Componentes Principales y Biplots con R
# Aprendizaje Máquina no Supervisado
#
# Este script está ajustado a lo que solicita el PDF:
# 1) Biplot GH' de Johnson y Wichern mediante la definición y funciones nativas.
# 2) Biplot de los mismos datos mediante PCA + fviz_pca_biplot.
# 3) Biplot GH' de los datos de CONAPO mediante la definición y funciones nativas.
# 4) Biplot de CONAPO mediante PCA + fviz_pca_biplot.
# 5) Comparación cualitativa.
################################################################################

# ---------------------------------------------------------------------------
# PARTE 1. DATOS DEL EJERCICIO 1.16 DE JOHNSON Y WICHERN
# ---------------------------------------------------------------------------

X <- matrix(c(
1.103,1.052,2.139,2.238,0.873,0.872,
0.842,0.859,1.873,1.741,0.590,0.744,
0.925,0.873,1.887,1.809,0.767,0.713,
0.857,0.744,1.739,1.547,0.706,0.674,
0.795,0.809,1.734,1.715,0.549,0.654,
0.787,0.779,1.509,1.474,0.782,0.571,
0.933,0.880,1.695,1.656,0.737,0.803,
0.799,0.851,1.740,1.777,0.618,0.682,
0.945,0.876,1.811,1.759,0.853,0.777,
0.921,0.906,1.954,2.009,0.823,0.765,
0.792,0.825,1.624,1.657,0.686,0.668,
0.815,0.751,2.204,1.846,0.678,0.546,
0.755,0.724,1.508,1.458,0.662,0.595,
0.880,0.866,1.786,1.811,0.810,0.819,
0.900,0.838,1.902,1.606,0.723,0.677,
0.764,0.757,1.743,1.794,0.586,0.541,
0.733,0.748,1.863,1.869,0.672,0.752,
0.932,0.898,2.028,2.032,0.836,0.805,
0.856,0.786,1.390,1.324,0.578,0.610,
0.890,0.950,2.187,2.087,0.758,0.718,
0.688,0.532,1.650,1.378,0.533,0.482,
0.940,0.850,2.334,2.225,0.757,0.731,
0.493,0.616,1.037,1.268,0.546,0.615,
0.835,0.752,1.509,1.422,0.618,0.664,
0.915,0.936,1.971,1.869,0.869,0.868
), nrow=25, ncol=6, byrow=TRUE)

colnames(X) <- c(
  "Dom.radio", "Radio",
  "Dom.humero", "Humero",
  "Dom.cubito", "Cubito"
)

# ---------------------------------------------------------------------------
# PARTE 1. GH'-BIPLOT CON LA DEFINICIÓN Y FUNCIONES NATIVAS
# ---------------------------------------------------------------------------
# IMPORTANTE:
# El PDF define X como una matriz CENTRADA.
# No se estandariza X para esta figura.

n <- nrow(X)

# Centrado por vector de medias
Xc <- scale(X, center = TRUE, scale = FALSE)

# DVS: X = U Lambda V'
d <- svd(Xc)

# Matrices G y H exactamente como aparecen en el PDF:
# G = sqrt(n)[u1 u2]
# H = (1/sqrt(n))[v1 v2] diag(lambda1,lambda2)

G <- sqrt(n) * d$u[, 1:2]

H <- (1 / sqrt(n)) *
     d$v[, 1:2] %*%
     diag(d$d[1:2])

# Graficar los individuos
plot(
  G,
  type = "n",
  asp = 1,
  xlab = "PC1",
  ylab = "PC2",
  main = "GH'-biplot"
)

# Puntos azules y numeración
points(
  G[,1], G[,2],
  pch = 19,
  col = "blue"
)

text(
  G[,1], G[,2],
  labels = 1:n,
  pos = 3,
  cex = 0.75,
  col = "black"
)

# Escala de las flechas para que sean visibles,
# manteniendo su dirección y proporción.
escala <- 5

# Flechas rojas = variables
for (j in 1:ncol(X)) {

  arrows(
    0, 0,
    H[j,1] * escala,
    H[j,2] * escala,
    col = "red",
    length = 0.08,
    lwd = 1.2
  )

  text(
    H[j,1] * escala,
    H[j,2] * escala,
    labels = colnames(X)[j],
    col = "red",
    cex = 0.70,
    pos = ifelse(H[j,2] >= 0, 3, 1),
    offset = 0.3
  )
}

# ---------------------------------------------------------------------------
# PARTE 2. PCA + fviz_pca_biplot PARA LOS MISMOS DATOS
# ---------------------------------------------------------------------------

# FactoMineR
# scale.unit = FALSE porque el PDF usa la matriz centrada,
# no una nueva estandarización para este ejercicio.
library(FactoMineR)
library(factoextra)

res.pca <- PCA(
  X,
  scale.unit = FALSE,
  graph = FALSE
)

fviz_pca_biplot(
  res.pca,
  geom.ind = "point",
  col.ind = "cos2",
  gradient.cols = c("blue", "orange", "red"),
  col.var = "contrib",
  repel = TRUE,
  title = "GH'-biplot",
  xlab = paste0(
    "Dim1 (", round(res.pca$eig[1,2], 1), "%)"
  ),
  ylab = paste0(
    "Dim2 (", round(res.pca$eig[2,2], 1), "%)"
  )
)

# ---------------------------------------------------------------------------
# PARTE 3. CONAPO: GH'-BIPLOT CON LA DEFINICIÓN
# ---------------------------------------------------------------------------

library(readxl)

# Si tu IM.xlsx está en otra carpeta, cambia únicamente esta línea.
datos <- read_excel("IM.xlsx")

# Las columnas 5:13 son los 9 indicadores usados en la Actividad 4.
Xconapo <- datos[, 5:13]

# Convertir a matriz numérica
Xconapo <- as.data.frame(
  lapply(Xconapo, as.numeric)
)

Xconapo <- as.matrix(Xconapo)

# Los datos de la Actividad 4 se deben utilizar estandarizados.
Zconapo <- scale(Xconapo)

nC <- nrow(Zconapo)

# DVS
dC <- svd(Zconapo)

# G y H según la definición del PDF
GC <- sqrt(nC) * dC$u[,1:2]

HC <- (1 / sqrt(nC)) *
      dC$v[,1:2] %*%
      diag(dC$d[1:2])

# Porcentaje de varianza explicado
var_exp <- (dC$d^2 / nC) /
           sum(dC$d^2 / nC) * 100

# Biplot
plot(
  GC,
  type = "n",
  asp = 1,
  xlab = paste0(
    "PC1 (", round(var_exp[1],2), "%)"
  ),
  ylab = paste0(
    "PC2 (", round(var_exp[2],2), "%)"
  ),
  main = "GH'-biplot CONAPO"
)

# Puntos = municipios
points(
  GC[,1], GC[,2],
  pch = 19,
  col = "blue",
  cex = 0.55
)

# Etiquetas de municipios
text(
  GC[,1], GC[,2],
  labels = datos[[3]],
  pos = 3,
  cex = 0.35
)

# Escala de flechas
escalaC <- 5

for (j in 1:ncol(Zconapo)) {

  arrows(
    0, 0,
    HC[j,1] * escalaC,
    HC[j,2] * escalaC,
    col = "red",
    length = 0.07,
    lwd = 1.2
  )

  text(
    HC[j,1] * escalaC,
    HC[j,2] * escalaC,
    labels = colnames(Zconapo)[j],
    col = "red",
    cex = 0.55
  )
}

# ---------------------------------------------------------------------------
# PARTE 4. CONAPO: PCA + fviz_pca_biplot
# ---------------------------------------------------------------------------

# Como Zconapo YA está estandarizada, no volver a escalar.
res.conapo <- PCA(
  Zconapo,
  scale.unit = FALSE,
  graph = FALSE
)

fviz_pca_biplot(
  res.conapo,
  geom.ind = "point",
  col.ind = "cos2",
  gradient.cols = c("blue", "orange", "red"),
  col.var = "contrib",
  repel = TRUE,
  title = "GH'-biplot CONAPO",
  xlab = paste0(
    "Dim1 (", round(res.conapo$eig[1,2], 2), "%)"
  ),
  ylab = paste0(
    "Dim2 (", round(res.conapo$eig[2,2], 2), "%)"
  )
)

# ---------------------------------------------------------------------------
# PARTE 5. COMPARACIÓN CUALITATIVA
# ---------------------------------------------------------------------------

cat("\n================ COMPARACIÓN ================\n")
cat("Biplot por definición:\n")
cat("- Se construye directamente con la DVS mediante G y H.\n")
cat("- Los puntos representan individuos/municipios.\n")
cat("- Las flechas representan variables.\n")
cat("- La dirección de las flechas permite interpretar asociaciones.\n\n")

cat("Biplot con PCA + fviz_pca_biplot:\n")
cat("- Parte del mismo ACP.\n")
cat("- Presenta los individuos y variables en una visualización más elaborada.\n")
cat("- Usa cos2 para evaluar la calidad de representación de los individuos.\n")
cat("- Usa contrib para mostrar la importancia de las variables.\n\n")

cat("Conclusión:\n")
cat("- Ambos biplots deben mostrar una estructura cualitativamente equivalente.\n")
cat("- Las diferencias principales corresponden a la escala y presentación gráfica.\n")
cat("==============================================\n")

################################################################################
# FIN DEL SCRIPT
################################################################################
