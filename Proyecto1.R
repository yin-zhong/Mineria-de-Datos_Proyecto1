library(ggplot2)
library(factoextra)
library(FactoMineR)
library(tidyverse)
library(Rtsne)
library(dbscan)
library(dplyr)
library(cluster)
set.seed(216)

#elegir file
filename = file.choose()
data = readRDS(filename)
data <- data[!duplicated(data$track_id),]

#preguntar al usuario como quiere ingresar la canción
pregunta = readline(prompt = "Para crear su lista, marque 1 si desea ingresar el nombre de la cancíón y el nombre del artista/grupo o 2 si desea ingresar el id de la canción. Elija marcando 1 o 2 ")

if (pregunta == 1){
  pregunta2 = readline(prompt = "Cuál es el nombre de la canción? ")
  pregunta3 = readline(prompt = "Cuál es el nombre del artista/grupo? ")
  data1 <- filter(data, track_name == pregunta2 & artist_name == pregunta3)
}

if (pregunta == 2){
  pregunta4 = readline(prompt = "Cuál es el id de la canción? ")
  data1 <- filter(data, track_id == pregunta4)
}

#tomar muestra aleatoria de 10000
data_sample <- data[sample(nrow(data), 10000), ]

#agregar cancion ingresada por el usuario
data_sample[nrow(data_sample) + 1,] = data1

#seleccionar columnas para hacer el analisis
data_parametros <-  select(data_sample, track_id, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo)

#borrar datos invalidos y repetidos
colSums(is.na(data_parametros))
data_parametros <- na.omit(data_parametros)
data_parametros <- data_parametros[!duplicated(data_parametros$track_id),]

#normalizar
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
parametros_norm <- as.data.frame(lapply(data_parametros[,2:11], min_max_norm))

#############TSNE##############
data_tsne <- parametros_norm %>% 
  Rtsne(check_duplicates = FALSE) %>% 
  .$Y %>% 
  as.data.frame()

rownames(data_tsne) <- data_parametros$track_id

#vizualización
#ggplot(data_tsne, aes(V1, V2)) + 
#  geom_point(alpha = 0.5, color = "blue")

#calcular estadístico de hopkins
#get_clust_tendency(data_tsne, n = 30, graph = FALSE)

###############DBSCAN###############
#modelo_dbscan <- dbscan(data_tsne, eps = 3, minPts = 60)

#visualización
#ggplot(data_tsne, aes(V1, V2, col = factor(modelo_dbscan$cluster))) + 
#geom_point(alpha = 0.5) +
#theme(legend.position = "none") +
#scale_color_brewer(palette="Spectral")

#############KMEANS#################
kmeans_modelo <- kmeans(data_tsne, 10, iter.max  = 100)
kmeans_modelo

#vizualización
#fviz_cluster(kmeans_modelo, data = data_tsne, geom = c("point"),ellipse.type = "euclid")

##EVALUACION KMEANS
clusters <- kmeans_modelo$cluster
coefSil <- silhouette(clusters, dist(data_tsne))
summary(coefSil)

#visualizar el coeficiente de silueta de cada cluster
#fviz_silhouette(coefSil) + coord_flip()

#calcular el k ideal para el modelo
siluetas <- numeric(30)

for (k in 2:30){
  modelo <- kmeans(data_tsne, centers = k)
  temp <- silhouette(modelo$cluster, dist(data_tsne))
  siluetas[k] <- mean(temp[,3])
}
tempDF <- data.frame(CS=siluetas, K=c(1:30))

#visualizar el gráfico con el k ideal
#ggplot(tempDF, aes(x=K, y=CS)) + 
#geom_line() +
#scale_x_continuous(breaks=c(1:30)) +
#geom_vline(xintercept = which(tempDF$CS == max(tempDF$CS)), col = "red")

########KMEANS 2#########
#crear otro modelo de kmeans, cambiando k por el valor encontrado en la evaluacion anterior
kmeans_modelo2 <- kmeans(data_tsne, which(tempDF$CS == max(tempDF$CS)), iter.max  = 100)
kmeans_modelo2

#fviz_cluster(kmeans_modelo2, data = data_tsne, geom = c("point"),ellipse.type = "euclid")

##EVALUACION KMEANS 2
clusters2 <- kmeans_modelo2$cluster
coefSil2 <- silhouette(clusters2, dist(data_tsne))
summary(coefSil2)

#fviz_silhouette(coefSil2) + coord_flip()

#######PLAYLIST########
#crear columna de cluster de cada cancion
clusteres <- data.frame(kmeans_modelo2$size, kmeans_modelo2$centers)
data_kmeans <- data.frame(cluster = kmeans_modelo2$cluster, data_parametros)

data_final <- merge(data_kmeans, data_sample, by="track_id")
data_final <- select(data_final, cluster, track_id, track_name, artist_id, artist_name, duration_ms)

if (pregunta == 1){
  n_fila = which(data_final$track_name == pregunta2)
  n_cluster = data_final$cluster[n_fila]
  cancion_selec <- filter(data_final, track_name == pregunta2)
  data_final <- data_final[-c(n_fila), ]
}

if (pregunta == 2){
  n_fila = which(data_final$track_id == pregunta4)
  n_cluster = data_final$cluster[n_fila]
  cancion_selec <- filter(data_final, track_id == pregunta4)
  data_final <- data_final[-c(n_fila), ]
} 

cluster_cancion <- filter(data_final, cluster == n_cluster)
playlist <- cluster_cancion[sample(nrow(cluster_cancion), 60), ]
playlist <- select(playlist, track_name, artist_name, duration_ms)
cancion_selec <- select(cancion_selec, track_name, artist_name, duration_ms)
rownames(playlist) = seq(length=nrow(playlist))

print("Canción seleccionada: ")
print.data.frame(cancion_selec)

print("Playlist de canciones similares: ")
print.data.frame(playlist)
