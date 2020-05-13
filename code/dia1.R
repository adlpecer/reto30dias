
# 30 dias de graficos
# dia 1 - grafico de barras
# Autor: Adolfo Lopez

# Dependencias
library(dplyr)
library(spotifyr)
library(ggplot2)

# Credenciales personales para la API de Spotify
Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXX')

access_token <- get_spotify_access_token()

# Generos de interes
genres <- c("rock","pop","electronic","hip-hop",
             "trap","R&B","classical","metal",
             "jazz","blues","folk","country")
Popularity <- rep(0,12)
df <- data.frame(genres,Popularity)

# Bucle para extraer la popularidad por género
for (gen in genres) {
  art = get_genre_artists(gen, limit=50, authorization = access_token)
  df[df$genres == gen,2] = mean(art$popularity)
}

# Representación visual
p<-ggplot(data=df, aes(x=reorder(genres, Popularity), y=Popularity, fill=Popularity))+
  geom_bar(stat="identity") +
  coord_flip(ylim=c(50,100)) +
  labs(title="Día1 - Popularidad de géneros musicales en Spotify",
       subtitle = "Valor medio para los 50 artistas más populares por género",
       x = "Géneros",
       y = "Popularidad"
  ) +
  guides(fill=FALSE) +
  theme(legend.position = "none") +
  theme_minimal()+ 
  geom_text(aes(x = genres, 
                y = Popularity + 0.02, label = round(Popularity, 2)),
            hjust = -0.5)

# Salvar gráfico
ggsave("dia1-barras.png",width = 8,height = 7)

