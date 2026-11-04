# ============================================================
# PROYECTO: SEGMENTACIÓN DE CLIENTES
# Dataset: Mall Customers
# ============================================================

# 1. INSTALACIÓN Y CARGA DE PAQUETES --------------------------

paquetes <- c("ggplot2", "cluster", "dplyr")

paquetes_faltantes <- paquetes[
  !(paquetes %in% rownames(installed.packages()))
]

if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}

library(ggplot2)
library(cluster)
library(dplyr)


# 2. CARGA DEL DATASET ----------------------------------------

# El archivo Mall_Customers.csv debe estar en el directorio de trabajo.
# Puedes consultar el directorio actual con getwd().

clientes <- read.csv(
  "Mall_Customers.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Primeras filas
head(clientes)

# Dimensiones, estructura y resumen estadístico
dim(clientes)
str(clientes)
summary(clientes)

# Nombres originales de las columnas
names(clientes)


# 3. LIMPIEZA Y PREPARACIÓN -----------------------------------

# Renombrar columnas
# El dataset puede incluir "Genre" o "Gender" según la versión descargada.

names(clientes) <- c(
  "CustomerID",
  "Genre",
  "Age",
  "AnnualIncome",
  "SpendingScore"
)

# Comprobar valores ausentes
colSums(is.na(clientes))

# Eliminar filas duplicadas
clientes <- clientes[!duplicated(clientes), ]

# Eliminar filas con valores ausentes, si existieran
clientes <- na.omit(clientes)

# Codificar Genre:
# Male = 1
# Female = 0

clientes$GenreNumeric <- ifelse(
  tolower(trimws(clientes$Genre)) == "male",
  1,    title = "Distribución de la edad",
  x = "Edad",
  y = "Frecuencia"
) +
  theme_minimal()

# Distribución por género
ggplot(clientes, aes(x = Genre, fill = Genre)) +
  geom_bar() +
  labs(
    title = "Distribución de clientes por género",
    x = "Género",
    y = "Número de clientes"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Distribución de ingresos anuales
ggplot(clientes, aes(x = AnnualIncome)) +
  geom_histogram(
    binwidth = 10,
    fill = "darkgreen",
    color = "white"
  ) +
  labs(
    title = "Distribución de los ingresos anuales",
    x = "Ingresos anuales (k$)",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Distribución de la puntuación de gasto
ggplot(clientes, aes(x = SpendingScore)) +
  geom_histogram(
    binwidth = 10,
    fill = "orange",
    color = "white"
  ) +
  labs(
    title = "Distribución de la puntuación de gasto",
    x = "Puntuación de gasto",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Diagramas de caja para detectar posibles valores extremos
ggplot(clientes, aes(y = AnnualIncome)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Diagrama de caja de los ingresos anuales",
    y = "Ingresos anuales (k$)"
  ) +
  theme_minimal()

ggplot(clientes, aes(y = SpendingScore)) +
  geom_boxplot(fill = "salmon") +
  labs(
    title = "Diagrama de caja de la puntuación de gasto",
    y = "Puntuación de gasto"
  ) +
  theme_minimal()
# 5. NORMALIZACIÓN DE VARIABLES -------------------------------

# Se utilizan ingresos y puntuación de gasto para crear los clusters.
# scale() transforma cada variable para que tenga media 0
# y desviación típica 1.

datos_cluster <- scale(
  clientes[, c("AnnualIncome", "SpendingScore")]
)

# Comprobar normalización
head(datos_cluster)
summary(datos_cluster)


# 6. K-MEANS: MÉTODO DEL CODO ---------------------------------

# Fijar semilla para obtener resultados reproducibles
set.seed(123)

# Probar entre 2 y 10 clusters
valores_k <- 2:10

wss <- sapply(valores_k, function(k) {
  modelo <- kmeans(
    datos_cluster,
    centers = k,
    nstart = 25
  )
  
  modelo$tot.withinss
})

# Tabla de resultados del método del codo
resultados_codo <- data.frame(
  NumeroClusters = valores_k,
  WSS = wss
)

print(resultados_codo)

# Gráfico del método del codo
ggplot(
  resultados_codo,
  aes(x = NumeroClusters, y = WSS)
) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "red", size = 3) +
  scale_x_continuous(breaks = valores_k) +
  labs(
    title = "Método del codo para K-Means",
    x = "Número de clusters",
    y = "Suma de cuadrados dentro de los clusters"
  ) +
  theme_minimal()
)

# Silueta del modelo jerárquico
silueta_jerarquico <- silhouette(
  clusters_jerarquicos,
  matriz_distancias
)

promedio_silueta_jerarquico <- mean(
  silueta_jerarquico[, "sil_width"]
)

# Comparación de resultados
comparacion_modelos <- data.frame(
  Modelo = c("K-Means", "Jerárquico"),
  SiluetaPromedio = c(
    promedio_silueta_kmeans,
    promedio_silueta_jerarquico
  )
)

print(comparacion_modelos)

# Gráficos de silueta
plot(
  silueta_kmeans,
  main = "Silueta del modelo K-Means",
  col = 2:(k_optimo + 1)
)

plot(
  silueta_jerarquico,
  main = "Silueta del clustering jerárquico",
  col = 2:(k_optimo + 1)
)

# Seleccionar el mejor modelo
if (promedio_silueta_kmeans > promedio_silueta_jerarquico) {
  mejor_modelo <- "K-Means"
  clientes$ClusterFinal <- clientes$ClusterKMeans
} else {
  mejor_modelo <- "Clustering jerárquico"
  clientes$ClusterFinal <- clientes$ClusterJerarquico
}

cat(
  "\nEl modelo seleccionado es:",
  mejor_modelo,
  "\n"
)

cat(
  "Silueta media de K-Means:",
  round(promedio_silueta_kmeans, 3),
  "\n"
)

cat(
  "Silueta media del modelo jerárquico:",
  round(promedio_silueta_jerarquico, 3),
  "\n"
)
)],
by = list(Cluster = clientes$ClusterKMeans),
FUN = mean
)

perfil_kmeans$NumeroClientes <- as.numeric(
  table(clientes$ClusterKMeans)
)

perfil_kmeans <- perfil_kmeans %>%
  mutate(
    across(
      c(Age, AnnualIncome, SpendingScore, GenreNumeric),
      ~ round(.x, 2)
    )
  )

print(perfil_kmeans)

# Perfiles de los clusters jerárquicos
perfil_jerarquico <- aggregate(
  clientes[, c(
    "Age",
    "AnnualIncome",
    "SpendingScore",
    "GenreNumeric"
  )],
  by = list(Cluster = clientes$ClusterJerarquico),
  FUN = mean
)

perfil_jerarquico$NumeroClientes <- as.numeric(
  table(clientes$ClusterJerarquico)
)

perfil_jerarquico <- perfil_jerarquico %>%
  mutate(
    across(
      c(Age, AnnualIncome, SpendingScore, GenreNumeric),
      ~ round(.x, 2)
    )
  )

print(perfil_jerarquico)

# Perfil del modelo seleccionado
perfil_final <- aggregate(
  clientes[, c(
    "Age",
    "AnnualIncome",
    "SpendingScore",
    "GenreNumeric"
  )],
  by = list(Cluster = clientes$ClusterFinal),
  FUN = mean
)

perfil_final$NumeroClientes <- as.numeric(
  table(clientes$ClusterFinal)
)

perfil_final <- perfil_final %>%
  mutate(
    across(
      c(Age, AnnualIncome, SpendingScore, GenreNumeric),
      ~ round(.x, 2)
    )
  )

print(perfil_final)

# 12. INTERPRETACIÓN AUTOMÁTICA DE LOS SEGMENTOS --------------

media_ingresos <- mean(clientes$AnnualIncome)
media_gasto <- mean(clientes$SpendingScore)
media_edad <- mean(clientes$Age)

interpretar_segmento <- function(edad, ingresos, gasto) {
  
  nivel_edad <- ifelse(
    edad < media_edad,
    "clientes relativamente jóvenes",
    "clientes de mayor edad"
  )
  
  nivel_ingresos <- ifelse(
    ingresos < media_ingresos,
    "ingresos inferiores a la media",
    "ingresos superiores a la media"
  )
  
  nivel_gasto <- ifelse(
    gasto < media_gasto,
    "nivel de gasto bajo",
    "nivel de gasto alto"
  )
  
  paste(
    nivel_edad,
    "con",
    nivel_ingresos,
    "y",
    nivel_gasto
  )
}

perfil_final$Interpretacion <- mapply(
  interpretar_segmento,
  perfil_final$Age,
  perfil_final$AnnualIncome,
  perfil_final$SpendingScore
)

print(perfil_final)


# 13. EXPORTACIÓN DE RESULTADOS -------------------------------

# Guardar el dataset con los clusters asignados
write.csv(
  clientes,
  "Mall_Customers_Clusters.csv",
  row.names = FALSE
)

# Guardar el perfil de los segmentos
write.csv(
  perfil_final,
  "Perfil_Segmentos.csv",
  row.names = FALSE
)

cat(
  "\nAnálisis finalizado correctamente.",
  "\nSe han generado los archivos:",
  "\n- Mall_Customers_Clusters.csv",
  "\n- Perfil_Segmentos.csv\n"
)

