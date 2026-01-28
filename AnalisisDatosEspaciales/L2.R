# Construyendo la matriz W
library(sp); library(spdep); library(splm); library(plm)
library(RColorBrewer); library(classInt); library(lattice)
library(stargazer); library(sf); library(maptools); library(tmap) 
library(tmaptools); library(tidyverse); library(summarytools)
library(rgeoda); library(tidylog); library(spatialreg); library(rgdal)
library(ggview)

setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometría espacial/Talleres/Taller1/2026-I")

# Cargando los datos
data<-read.csv("data.csv", header = TRUE, sep = ",", dec=".")
names(data)

ggplot(data, aes(x = fct_reorder(names, gva), y = gva)) + 
  geom_segment(aes(x = fct_reorder(names, gva), xend = names,
                   y = 0, yend = gva), color = "gray75")+ 
  geom_point(size = 2.5, aes(colour = gva)) +
  scale_colour_viridis_c("", limits = c(min(data$gva), max(data$gva)),
                         direction = -1) +
  geom_hline(yintercept = median(data$gva), colour = "#3D205E", alpha = 0.5) +
  annotate("text", label = paste0("Median = ", round(median(data$gva),2)), 
           x = 10, y = round(median(data$gva),2), size = 3, alpha = 0.5,   
           colour = "#3D205E", angle = 90, vjust = 1.5, hjust = 4.7)   + 
  coord_flip(ylim = c(min(data$gva), max(data$gva))) + 
  labs(title = "Gross Value Added in UK (% country)", x = "", y = "") +
  theme(axis.title.x = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.key.size = unit(0.3, "cm"), 
        legend.key.width = unit(0.3,"cm"),
        legend.text=element_text(size=7),
        legend.position = c(0.9,0.5),
        legend.background = element_rect(fill=NA)) +
  canvas(7,5, units = "in")

ggsave("f1.png", width = 7, height = 5, dpi = 300, bg="white")

# Cargando el mapa 
UK12 <- st_read("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometría espacial/Talleres/Taller1/2017-II/UK12RS.shp")
names(UK12)

tm_shape(UK12) +
  tm_polygons(col = 'gray95', border.col = "gray90") +
  tm_text("nuts118nm", size=.4)

ggplot(UK12) + 
  geom_sf(data=UK12, colour = "gray95", fill = "gray90") +
  geom_sf_text(aes(label = nuts118nm), size=1.5, colour = "black") +
  theme_void() +
  canvas(3,5, units = "in")

# Pegando los datos al mapa, utilizando la llave FIPS
summarytools::freq(data$region)
summarytools::freq(UK12$nuts118nm)

data <- data |> 
  mutate(nuts118nm = case_when(region=="East Anglia"~"East of England",
                               region=="East Midlands"~"East Midlands (England)",
                               region=="Greater London"~"London",
                               region=="North of England"~"North East (England)",
                               region=="North West England"~"North West (England)",
                               region=="Northern Ireland  "~"Northern Ireland",
                               region=="South East England"~"South East (England)",
                               region=="South West England"~"South West (England)",
                               region=="West Midlands"~"West Midlands (England)",
                               region=="Yorkshire & Humberside"~"Yorkshire and The Humber",
                               TRUE ~ region))
summarytools::freq(data$nuts118nm)

dataUK <- merge(UK12,data)

# Mapiando gva
ggplot(dataUK) + 
  geom_sf(aes(fill = gva), color = "gray95", size = 0.05) +
  geom_sf_text(aes(label = nuts118nm), size=1.5, colour = "black") +
  scale_fill_viridis_c(direction = -1) +
  labs(fill="GVA in UK (% country)") +
  theme_void() +
  theme(legend.position = c(.2,.2),
        legend.key.size = unit(0.4,"cm"),
        legend.text=element_text(size=4.5),
        legend.title=element_text(size=5))+
  canvas(3,5, units = "in")

ggsave("f2.png", width = 3, height = 5, units = "cm", dpi = 300)

tmap_mode("view")
tm_shape(dataUK) +
  tm_polygons("gva", title="GVA", palette="Reds", id="nuts118nm", n=4,style="equal") + 
  tm_text("nuts118nm", col="gray95", scale=.9) + 
  tm_basemap(server="http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",alpha=0.5) +
  tm_view(view.legend.position = c("right", "bottom"))

tm_shape(dataUK) +
  tm_polygons("gva", title="GVA", palette="Reds", id="nuts118nm", breaks=c(1.5,13,24,47)) + 
  tm_text("nuts118nm", col="gray95", scale=.9) + 
  tm_basemap(server="http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",alpha=0.5) +
  tm_view(view.legend.position = c("right", "bottom"))

tmap_mode("plot")
tmap_save(filename = "mymap.png",
          units = "cm", dpi = 300)

# Webs de mapas: https://geocompr.robinlovelace.net/adv-map.html
#                https://spatialanalysis.github.io/lab_tutorials/4_R_Mapping.html

# Construyendo la matriz de pesos espaciales 
# Tipo Queen
nb <- poly2nb(UK12,queen=T)
nb

UK11 <- UK12 |> 
  filter(!nuts118nm %in% c("Northern Ireland"))

ggplot(UK11) + 
  geom_sf(data=UK11, colour = "gray95", fill = "gray90") +
  geom_sf_text(aes(label = nuts118nm), size=1.5, colour = "black") +
  theme_void() +
  canvas(3,5, units = "in")

nb <- poly2nb(UK11,queen=T)
nb

We <- nb2listw(nb, style="W")
names(We)
We$weights

# Graficando la contiguidad
cnt11 <- st_geometry(st_centroid(UK11)) # construyendo los centroides

line_w <- nb2lines(nb, coords = st_geometry(st_centroid(UK11)), as_sf = TRUE)

ggplot() +
  geom_sf(data=UK11, colour = "gray95", fill = "gray90") +
  geom_sf(data=cnt11, color="red")  +
  geom_sf(data = line_w, fill = "grey40", size=.4) +
  theme_void() +
  canvas(3,5, units = "in")

# 6 vecinos más cercanos
coords <- st_centroid(st_geometry(UK12), of_largest_polygon=TRUE)
plot(coords)
knn6 <- knearneigh(coords, k=6)
nb_knn6 <- knn2nb(knn6)
We_knn6 <- nb2listw(nb_knn6, style = "W")
names(We_knn6)
We_knn6$weights

cnt12 <- st_geometry(st_centroid(UK12)) # construyendo los centroides

line_knn6 <- nb2lines(nb_knn6, coords = st_geometry(st_centroid(UK12)), as_sf = TRUE)

ggplot() +
  geom_sf(data=UK12, colour = "gray95", fill = "gray90") +
  geom_sf(data=cnt12, color="red")  +
  geom_sf(data = line_knn6, fill = "grey40", size=.4) +
  theme_void() +
  canvas(3,5, units = "in")

# Con distancia
# Primero detectamos las distancia máxima que hay entre centroides
k1 <- knn2nb(knearneigh(coords))
critical.threshold <- max(unlist(nbdists(k1,coords)))
critical.threshold

nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
summary(nb.dist.band, coords)
We_dis = nb2listw(nb.dist.band, style = "W")

line_dis <- nb2lines(nb.dist.band, coords = st_geometry(st_centroid(UK12)), as_sf = TRUE)

ggplot() +
  geom_sf(data=UK12, colour = "gray95", fill = "gray90") +
  geom_sf(data=cnt12, color="red")  +
  geom_sf(data = line_dis, fill = "grey40", size=.4) +
  theme_void() +
  canvas(3,5, units = "in")
