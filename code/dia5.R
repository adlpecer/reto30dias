
# 30 dias de graficos
# dia 5 - grafico de arcos
# Autor: Adolfo Lopez

# Dependencias
library(spotifyr)
library(arcdiagram)
library(igraph)

# Credenciales personales para la API de Spotify
Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXX')

access_token <- get_spotify_access_token()

# ID del grupo inicial
bonobo <- "0cmWgDlu9CwTgxPhf403hb"

# Artistas relacionados con el grupo inicial
seed.df <- get_related_artists(bonobo, authorization = access_token)
seed.df <- seed.df[,c("id","name")]

bands <- c("Bonobo",seed.df$name)
# Matriz de adyacencias
c.matrix <- matrix(nrow=21, ncol=21,
                   dimnames = list(bands, bands))

c.matrix[is.na(c.matrix)] <- 0

# Rellenando la matriz
for (band in bands[-1]){ 
  c.matrix["Bonobo",band] <- 1
  c.matrix[band,"Bonobo"] <- 1
  }

for (band in bands[-1]) {
  artist <- seed.df[seed.df$name==band,"id"]
  df <- get_related_artists(artist, authorization = access_token)
  df <- df[,c("id","name")]
  n.list <- df$name
  for (n in n.list) {
    if (n %in% colnames(c.matrix)) {
      c.matrix[n,band] <- c.matrix[n,band] + 1
      c.matrix[band,n] <- c.matrix[band,n] + 1
    }
  }
}

# Ordenar por numero de conexiones
sorted.bands <- colSums(c.matrix)
sorted.bands <- sort(sorted.bands,decreasing = T)
sorted.bands <- names(sorted.bands)
sorted.bands <- gsub(" ","\n",sorted.bands)

# De matriz a lista de aristas
g <- graph_from_adjacency_matrix(c.matrix, mode="undirected",weighted = T)
edgelist <- get.edgelist(g)
edgelist <- gsub(" ","\n",edgelist)

# Representacion
png("dia5-arcos.png",units = "in",width = 14, height = 6,res=600)
arcplot(edgelist, cex.labels = 0.9,
        lwd.arcs = 2,
        ordering = sorted.bands,
        cex.nodes = 0.2,
        col.nodes = "gray20",
        col.labels = "gray50")
dev.off()

