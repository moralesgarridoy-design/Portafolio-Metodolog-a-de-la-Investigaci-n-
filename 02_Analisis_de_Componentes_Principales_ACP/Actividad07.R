##################################################################################
#
# Universidad Veracruzana
# Licenciatura en Ingeniería en Ciencia de Datos
# Aprendizaje Máquina no Supervisado
# 
# Profesor: Sergio Juarez
# Fecha: 7 de septiembre, 2026
#
# Actividad 07
#
# Análisis de Componentes Principales (ACP) y biplot de los datos del 
# ejercicio 1.16, página 41 de Johnson & Whichern, (2014).
#
# Referencia
#
# Johnson, R., & Wichern, D. (2014). Applied Multivariate Statistical
# Analysis (6th ed.). Pearson.
# 
##################################################################################
#
# Ejercicio 1.16. 
# En un estudio para determinar si el ejercicio o suplementos alimenticios 
# reducirían la pérdida de hueso en mujeres mayores, el investigador midió
# el contenido de mineral en huesos por medio de absorptiometría. Se registraron
# mediciones para tres huesos en los lados dominants y no dominantes:
# 
# Radio dominante
# Radio
# Húmero dominante
# Húmero
# Cúbito dominante 
# Cúbito 
#
# Realiza un ACP y grafica un biplot de los datos.

# Solución
#
# Cargamos los datos de la Tabla 1.8 de Johnson, R., & Wichern, D. (2014).
#
X <- matrix(c(
1.103, 1.052, 2.139, 2.238, 0.873, 0.872,
0.842, 0.859, 1.873, 1.741, 0.590, 0.744,
0.925, 0.873, 1.887, 1.809, 0.767, 0.713,
0.857, 0.744, 1.739, 1.547, 0.706, 0.674,
0.795, 0.809, 1.734, 1.715, 0.549, 0.654,
0.787, 0.779, 1.509, 1.474, 0.782, 0.571,
0.933, 0.880, 1.695, 1.656, 0.737, 0.803,
0.799, 0.851, 1.740, 1.777, 0.618, 0.682,
0.945, 0.876, 1.811, 1.759, 0.853, 0.777,
0.921, 0.906, 1.954, 2.009, 0.823, 0.765,
0.792, 0.825, 1.624, 1.657, 0.686, 0.668,
0.815, 0.751, 2.204, 1.846, 0.678, 0.546,
0.755, 0.724, 1.508, 1.458, 0.662, 0.595,
0.880, 0.866, 1.786, 1.811, 0.810, 0.819,
0.900, 0.838, 1.902, 1.606, 0.723, 0.677,
0.764, 0.757, 1.743, 1.794, 0.586, 0.541,
0.733, 0.748, 1.863, 1.869, 0.672, 0.752,
0.932, 0.898, 2.028, 2.032, 0.836, 0.805,
0.856, 0.786, 1.390, 1.324, 0.578, 0.610,
0.890, 0.950, 2.187, 2.087, 0.758, 0.718,
0.688, 0.532, 1.650, 1.378, 0.533, 0.482,
0.940, 0.850, 2.334, 2.225, 0.757, 0.731,
0.493, 0.616, 1.037, 1.268, 0.546, 0.615,
0.835, 0.752, 1.509, 1.422, 0.618, 0.664,
0.915, 0.936, 1.971, 1.869, 0.869, 0.868
), nrow=25, ncol=6, byrow=TRUE)

# Nombre de las variables
colnames(X) <- c("Dom.radio","Radio","Dom.humero","Humero","Dom.cubito","Cubito")

# Dimensión de la matriz de datos
n <- nrow(X)
p <- ncol(X)

# Centramos a los datos
Xc <- scale(X, center=TRUE, scale=FALSE)   # para covarianza

# DVS de la matriz centrada
svd_X <- svd(Xc)

# Construir G y H
G <- sqrt(n) * svd_X$u[, 1:2]
H <- (1/sqrt(n)) * svd_X$v[, 1:2] %*% diag(svd_X$d[1:2])

# Biplot
#
# Hileras de G: Objetos como puntos (mujeres mayores)
plot(G, pch=19, col="blue", xlab="PC1", ylab="PC2",
     main="GH'-biplot")
text(G, labels=1:n, pos=3, cex=0.7)

# Hileras de H: variables como flechas
escala <- 5   # factor de escala para visualización
for (j in 1:ncol(X)) {
    arrows(0, 0, H[j,1]*escala, H[j,2]*escala,
           col="red", lwd=1, length=0.1)
    text(H[j,1]*escala*1.1, H[j,2]*escala*1.1,
         colnames(X)[j], col="red", cex=0.75)
}
#
##################################################################################
#
# Biplots con paquetes de R:

install.packages(c("factoextra", "FactoMineR"))
library(factoextra)
library(FactoMineR)

# ACP con la matriz de datos centrada
# lo que es equivalente al ACP de la
# matriz de varianzas y covarianzas.

res.pca <- PCA(X, scale.unit = FALSE, graph = FALSE)

# Graficamos el biplot:

fviz_pca_biplot(res.pca, 
                # 1. Customize Individuals (Rows of X)
                geom.ind = "point",             # Objetos
                col.ind = "cos2",               # Color de los objetos depende de su 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # Color de los objetos
                alpha.ind = 0.6,                
                
                # 2. Customize Variables (Columns of X)
                col.var = "contrib",            # Color de las variables depende de su peso  
                gradient.cols.var = "Blues",    # Paleta
                repel = TRUE,                   # Que no se traslapen los nombres de las variables
                
                # 3. Aesthetics
                title = "GH'-biplot",
                ggtheme = theme_minimal())      # Gráfica minimalista

# Interpretación del biplot con los colores.
#
# Para las hileras (puntos):
#
# El color de alta calidad (ej. Rojo/Naranja): Significa que el individuo 
# se "ajusta bien" al gráfico. Su posición actual representa fielmente su
# verdadera posición en el expacio de dimensión 6.
# Color de baja calidad (ej. Azul/Verde): Significa que ese individuo
# no se ajusta bien en los primeros dos ejes principales. En realidad, 
# puede estar en cualquiera de las otras dimensiones principales.
#
# Para las columnas (flechas):
#
# Las flechas cambian de color según su contribución significa que el color
# muestra la influencia de la variable en la conformación de los dos 
# primeros componentes principales. No todas las columnas aportan la misma
# cantidad de información. Algunas son sumamente explicativas y otras son
# casi irrelevantes (puro ruido). De acuerdo a la paleta, si la variable
# tiene una contribución alta, su flecha tiene un color intenso. Esto indica
# que la variable es fundamental en el biplot. Los primeros dos ejes 
# principales están determinados por los datos de esa variable. Si el color
# es de bajo tono, es decir, la flecha de la variable tiene un color tenue, 
# significa que esa variable tiene un impacto pequeño en la estructura
# geométrica de los datos. No aporta mucho a la diferenciación de los datos.

##################################################################################
#
# FIN
#
##################################################################################


