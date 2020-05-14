# 30 dias de graficos
# dia 3 - grafico de puntos
# Autor: Adolfo Lopez

# Dependencias
library(FlexParamCurve)
library(ggplot2)

# Carga y muestreo
df <- penguin.data
df <- df[df$year == 2000,]
mysample <- df[sample(1:nrow(df), 200, replace=FALSE),]

# Carga de imagen
img <- png::readPNG("./gunter.png")
g <- grid::rasterGrob(img)

# Representacion grafica
d<- ggplot(mysample, aes(x=ckage, y=weight)) +
      geom_point(size=1.5) +
      geom_smooth(formula = y ~ log(x))  +
      labs(title="Día3 - Gráfico de puntos",
          subtitle = "Masa corporal y edad de polluelos de pingüino azul",
          x = "Edad(días)",
          y = "Masa(gr)"
      ) + 
      theme_minimal()


# Para añadir el logo he utilizado código de:
# https://stackoverflow.com/questions/12463691/inserting-an-image-to-ggplot-outside-the-chart-area
size = unit(2, "cm")
# Set up the layout for grid 
heights = unit.c(size, unit(1, "npc") - size)
widths = unit.c(unit(1, "npc") - size, size)
lo = grid.layout(2, 2, widths = widths, heights = heights)
# Show the layout
grid.show.layout(lo)

# Position the elements within the viewports
grid.newpage()
pushViewport(viewport(layout = lo))

# The plot
pushViewport(viewport(layout.pos.row=1:2, layout.pos.col = 1:2))
print(d, newpage=FALSE)
popViewport()

# The logo
pushViewport(viewport(layout.pos.row=1, layout.pos.col = 2))
print(grid.draw(g), newpage=FALSE)
popViewport()
popViewport()

# To save the object
g = grid.grab()

grid.newpage()
grid.draw(g)
