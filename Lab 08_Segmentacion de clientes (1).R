# ============================================================
# SEGMENTACION DE CLIENTES - MALL CUSTOMERS
# ============================================================

library(ggplot2)
library(cluster)

# 1. CARGA Y EXPLORACION --------------------------------------

clientes <- read.csv("Mall_Customers.csv", check.names = FALSE)

head(clientes)
dim(clientes)
str(clientes)
summary(clientes)

# Renombrar columnas
names(clientes) <- c(
  "CustomerID", "Genre", "Age",
  "AnnualIncome", "SpendingScore"
)

# Comprobar valores ausentes
colSums(is.na(clientes))

# Eliminar duplicados y valores ausentes
clientes <- na.omit(clientes[!duplicated(clientes), ])

# Codificar genero: Male = 1, Female = 0
clientes$GenreNumeric <- ifelse(clientes$Genre == "Male", 1, 0)


# 2. EXPLORACION ----------------------------------------------

hist(clientes$Age,
     main = "Distribucion de edad",
     xlab = "Edad")

hist(clientes$AnnualIncome,
     main = "Distribucion de ingresos",
     xlab = "Ingresos")

hist(clientes$SpendingScore,
     main = "Distribucion de gasto",
     xlab = "Spending Score")


# 3. NORMALIZACION --------------------------------------------

# Variables utilizadas para formar los clusters
datos <- scale(
  clientes[, c("AnnualIncome", "SpendingScore")]
)

head(datos)


# 4. K-MEANS Y METODO DEL CODO --------------------------------

set.seed(123)

k <- 2:10

wss <- sapply(k, function(x) {
  kmeans(datos, centers = x, nstart = 25)$tot.withinss
})

plot(k, wss,
     type = "b",
     main = "Metodo del codo",
     xlab = "Numero de clusters",
     ylab = "WSS")

# El codo suele encontrarse alrededor de 5
k_optimo <- 5

set.seed(123)
km <- kmeans(datos, centers = k_optimo, nstart = 25)

clientes$ClusterKMeans <- factor(km$cluster)


# 5. CLUSTERING JERARQUICO ------------------------------------

distancias <- dist(datos, method = "euclidean")

hc <- hclust(distancias, method = "ward.D")

plot(hc,
     labels = FALSE,
     main = "Dendrograma")

rect.hclust(hc, k = k_optimo, border = 2:6)

clusters_hc <- cutree(hc, k = k_optimo)

clientes$ClusterJerarquico <- factor(clusters_hc)


# 6. EVALUACION CON SILUETA -----------------------------------

sil_km <- silhouette(km$cluster, distancias)
sil_hc <- silhouette(clusters_hc, distancias)

media_km <- mean(sil_km[, 3])
media_hc <- mean(sil_hc[, 3])

cat("Silueta K-Means:", round(media_km, 3), "\n")
cat("Silueta Jerarquico:", round(media_hc, 3), "\n")

if (media_km > media_hc) {
  cat("Mejor modelo: K-Means\n")
  clientes$ClusterFinal <- clientes$ClusterKMeans
} else {
  cat("Mejor modelo: Clustering Jerarquico\n")
  clientes$ClusterFinal <- clientes$ClusterJerarquico
}


# 7. PERFILES DE LOS SEGMENTOS --------------------------------

perfil <- aggregate(
  clientes[, c("Age", "AnnualIncome", "SpendingScore")],
  by = list(Cluster = clientes$ClusterFinal),
  FUN = mean
)

perfil[, 2:4] <- round(perfil[, 2:4], 2)

print(perfil)


# 8. VISUALIZACIONES ------------------------------------------

# K-Means
ggplot(clientes,
       aes(AnnualIncome, SpendingScore,
           color = ClusterKMeans)) +
  geom_point(size = 3) +
  labs(
    title = "Clusters K-Means",
    x = "Ingresos anuales (k$)",
    y = "Spending Score"
  ) +
  theme_minimal()

# Clustering jerarquico
ggplot(clientes,
       aes(AnnualIncome, SpendingScore,
           color = ClusterJerarquico)) +
  geom_point(size = 3) +
  labs(
    title = "Clusters Jerarquicos",
    x = "Ingresos anuales (k$)",
    y = "Spending Score"
  ) +
  theme_minimal()


# 9. RESULTADO FINAL ------------------------------------------

cat("\nPerfil medio de cada segmento:\n")
print(perfil)

cat("\nAnalisis finalizado correctamente.\n")
