# Análisis de datos espaciales

library(tidyverse); library(sf); library(ggview); library(ggnewscale); library(summarytools)

setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometría espacial/R/L3")

# Cargando el shapefile de barrios de Cali
barriosCali <- st_read("BARRIOS.shp")
crs <- st_crs(barriosCali)

ggplot() + 
  geom_sf(data=barriosCali, colour = "gray95", fill = "gray90") +
  theme_void() +
  canvas(3,5, units = "in")

# Cargando la base de datos
data<-read.csv("datar.csv", header = TRUE, sep = ",", dec=".")

# Volviendo los datos a puntos geográficos
points <- st_as_sf(data, 
                      coords = c("x", "y"),
                      crs = st_crs(crs))

# Mapa de densidad de la actividad económica y distribución de puntos
ggplot() +
  geom_sf(data = barriosCali, 
          aes(fill = "Barrio"), 
          color = "grey50", linewidth = 0.25) +
  scale_fill_manual(values = c("Barrio" = "grey95"), 
                    name = "",
                    guide = guide_legend(order = 1,
                      keywidth = unit(0.3, "cm"),   # Ancho del cuadrito
                      keyheight = unit(0.3, "cm"),  # Alto del cuadrito
                      override.aes = list(alpha = 0.1), # Asegura que el cuadro no sea transparente
                      label.theme = element_text(size = 6))) +
  geom_sf(data = points, color = "red", size = 0.05, alpha = .3) +
  new_scale_fill()+
  stat_density_2d(data = data,
                  aes(x = x, y = y, fill = after_stat(level)), # 'level' define el color por densidad
                  geom = "polygon",        # Crea áreas rellenas (como el _filled)
                  alpha = 0.4,             # Transparencia
                  color = NA,              # ELIMINA el borde de los niveles (evita el recuadro interno)
                  show.legend = T) +
  scale_fill_viridis_c(option = "viridis",
                       breaks = function(x) range(x), 
                       labels = c("Mín", "Máx"),
                       name = "Densidad",
                       guide = guide_colorbar(order = 1,
                                              barwidth = unit(0.3, "cm"),  # Reduce el ancho de la barra
                                              barheight = unit(.5, "cm"),   # Reduce la altura de la barra
                                              title.theme = element_text(size = 6), # Texto del título más pequeño
                                              label.theme = element_text(size = 6))) +  # Números de la leyenda más pequeños
  theme_void() +
  theme(legend.position = c(0.2, 0.2), # Ubicación de la leyenda
        legend.title = element_text(size = 10),
        legend.spacing.y = unit(-0.3, "cm")) +
  canvas(3,5, units = "in")
