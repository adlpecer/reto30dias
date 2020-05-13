
# 30 dias de graficos
# dia 2 - grafico de lineas
# Autor: Adolfo Lopez

# Dependencias
library(ggplot2)
library(reshape2)

# Carga y procesamiento del dataset
df <- read.csv("serie2019.csv")
colnames(df) <- c("FECHA","MaxCala","MinCala","MaxMur","MinMur")
df$Calamocha <- (df$MaxCala + df$MinCala) / 2
df$Murcia <- (df$MaxMur + df$MinMur) / 2
df$FECHA <- as.Date(df$FECHA,format="%d-%m-%y")
df <- df[,c("FECHA","Murcia","Calamocha")]
meltdf <- melt(df,id="FECHA")

# Representacion grafica
p <- ggplot(meltdf, aes(x=FECHA, y=value,colour=variable,group=variable)) +
  geom_line() +
  labs(title="Día2 - Gráfico de líneas",
       subtitle = "Temperatura media de Murcia y Calamocha durante 2019",
       x = "",
       y = "Temperatura(ºC)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Salvar gráfico
ggsave("dia2-lineas.png",width = 9,height = 7)
