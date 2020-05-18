# 30 dias de graficos
# dia 6 - gráfico de donut
# Autor: Adolfo Lopez


# Dependencias
library(ggplot2)
library(ggridges)
# Carga de dataset, descargado de:
# https://www.kaggle.com/trolukovich/steam-games-complete-dataset
df <- read.csv("steam_games.csv",stringsAsFactors = F)

# Filtrar columnas de interés
keep <- c("recent_reviews","genre")
df <- df[,keep]

# Transformar la columna de revisiones (texto) en número
df$recent_reviews <- gsub(".+\\(", "", df$recent_reviews)
df$recent_reviews <-  gsub("\\).+", "", df$recent_reviews)
df$recent_reviews <-  gsub(" user.+", "", df$recent_reviews)
df$recent_reviews <- gsub(",","",df$recent_reviews)
df$recent_reviews <- as.numeric(df$recent_reviews)
df <- df[!is.na(df$recent_reviews),]

# Extraer los géneros únicos
Categories <- paste(df$genre,collapse=",")
Categories <- strsplit(Categories,",")
Categories <- unlist(Categories)
Categories <- unique(Categories)
Categories <- Categories[1:12]

# Nuevo data frame con los datos preprocesados
DF <- data.frame(recent_reviews=numeric(),
                 genre=character())

for (cat in Categories){
  df.a <- df[grepl(cat,df$genre),]
  df.a$genre <- cat
  DF <- rbind(DF,df.a)
}

# Representacion
ggplot(DF, aes(x = recent_reviews, y = genre, fill = genre)) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none") +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5, alpha = 0.7) +
  scale_x_log10() +
  labs(title="Día7 - Ridgeline plot",
       subtitle = "Número de revisiones recientes de videojuegos en Steam por género",
       x = "Revisiones (escala log10)",
       y = "Géneros")

ggsave("dia7-ridgeline.png",width = 9,height = 7)

