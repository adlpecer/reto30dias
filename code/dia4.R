
# 30 dias de graficos
# dia 4 - facet plot
# Autor: Adolfo Lopez

# Dependencias
library(dplyr)
library(ggplot2)
library(reshape2)

# Lista de ficheros/contaminantes
c.list <- c("Dioxido_azufre","Dioxido_nitrogeno","Ozono",
            "Monoxido_carbono","Particulas_suspendidas_PM2.5",
            "Particulas_suspendidas_PM10")

# Carga de los csv para cada contaminante 
contaminantes <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(contaminantes) <- c("Fecha","variable","values","contaminante")
for (c in c.list){
  fname <- paste0(c,".csv")
  df <- read.csv(fname,sep = c(";","."),na.strings = c("","-"))
  df <- melt(df,id.vars = "Fecha")
  df$contaminante <- c
  contaminantes <- rbind(contaminantes,df)
}

contaminantes$Fecha <- as.Date(contaminantes$Fecha,"%d/%m/%Y")

# Etiqueta a insertar en grafico
ann_text <- data.frame(Fecha = as.Date("2020-03-12"),
                       value = 20,
                       lab = "Primer día de restricciones (11-03-2020)",
                       contaminante ="Dioxido_azufre",
                       variable = "X28079008")

# Representacion grafica
p <- ggplot(data=contaminantes,
       aes(x=Fecha, y = value,
           colour = contaminante,
           group = variable)) + 
     geom_line() +
     facet_grid(contaminante ~ ., scales = "free_y") +
     labs(title="Día4 - Gráfico de facetas",
       subtitle = "Impacto de las restricciones de movilidad en Madrid en la calidad del aire",
       x = "Fecha",
       y = "Contaminantes (ug/m3)"
       ) + 
     theme_minimal() +
     theme(legend.position = "none",
           strip.text.y.right = element_text(angle = 0))+
     geom_vline(xintercept=as.Date("2020-03-11"), colour="gray20") + 
     geom_label(data = ann_text,
                label = "Primer día de restricciones (11-03-2020)",
                color="gray20",
                vjust=0,
                hjust=0)


ggsave("dia4-facetas.png",width = 9,height = 7)  
