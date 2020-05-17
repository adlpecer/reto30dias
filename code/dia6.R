# 30 dias de graficos
# dia 6 - gráfico de donut
# Autor: Adolfo Lopez


# Dependencias
library(ggplot2)
library(dplyr)

# Cargar tabla y adaptar
df <- read.csv("2020-05-15casosporcentajessexosedad.csv")
df$Porcentaje <- NULL
colnames(df)<-c("Edad","Sexo","Casos")
df$Edad <- gsub("g","",df$Edad)
# Sumar por rango de edad
df <- df %>% 
  group_by(Edad) %>% 
  summarise(Casos = sum(Casos))
# Agrupar menores de 18 en un único rango
df[1,"Casos"] <- df[1,"Casos"] + df[2,"Casos"]
df[1,1] <- "-18"
df <- df[-2, ]

# La siguiente parte del código está basada en el ejemplo de:
# https://www.r-graph-gallery.com/128-ring-or-donut-plot.html

# Compute percentages
df$fraction <- df$Casos / sum(df$Casos)

# Compute the cumulative percentages (top of each rectangle)
df$ymax <- cumsum(df$fraction)

# Compute the bottom of each rectangle
df$ymin <- c(0, head(df$ymax, n=-1))

# Compute label position
df$labelPosition <- (df$ymax + df$ymin) / 2

# Compute a good label
df$label <- paste0(df$Edad, "\n value: ", df$Casos)

# Representacion
ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2, fill=Edad)) +
  geom_rect() +
  geom_label( x=4, aes(y=labelPosition, label=Casos), size=4,show.legend = F) +
  scale_fill_brewer(palette="Spectral") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  labs(title="Día6 - Gráfico de donut",
       subtitle = "Número de casos acumulados por rango de edad en la Comunidad Valenciana (15/05/2020)",
       x = "",
       y = ""
  ) +
  theme_void() +
  theme(legend.position="bottom")

ggsave("dia6-donut.png",width = 9,height = 7)
