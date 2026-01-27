# Trabjando con Shapefiles
library(sf); library(tmap); library(tidyverse); library(readxl); library(tmaptools)
library(viridis); library(summarytools); library(terra); library(ggview)
library(ggmap); library(mapview); library(leaflet)

setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometría espacial/R/L1/2026-I")

colmunic <- st_read("colmunic.shp")

ggplot(colmunic) +
  geom_sf(color = "blue", size = 0.05) +
  canvas(11,9, units = 'in')

coldep <- st_read("coldepto.shp") |> 
  filter(!NOMBRE_DPT %in% NA) # Elinamos San Andrés

ggplot(coldep) +
  geom_sf(color = "blue", size = 0.05) +
  canvas(11,9, units = 'in')

ggplot() +
  geom_sf(data=colmunic, aes(color = "Municipios"), fill = NA, linewidth = .5) +
  geom_sf(data = coldep, aes(color = "Departamentos"), fill = NA, linewidth = 1) +
  geom_sf_text(data = coldep, aes(label = NOMBRE_DPT), size=3, color="blue")+
  scale_color_manual(
    name = "",
    values = c("Departamentos" = "black", "Municipios" = "grey80"),
    labels = c("Departamentos", "Municipios")) +
  theme_void() +
  theme(legend.position = c("right"),
        legend.key.size = unit(.7,"cm"),
        legend.text=element_text(size=12),
        legend.key.spacing.y = unit(.15, 'cm')) +
  canvas(9.5,9, units = 'in')

ggsave("f1.png", width = 9.5, height = 9, dpi = 300, bg="white")

# Cargando los datos del PIB percapita departamental
data <- read_excel("anex-PIBDep-TotalDep-2024pr.xlsx", 
                   sheet = "Cuadro 3", range = "A9:V43") |> 
  rename("DPTO"="Código Departamento (DIVIPOLA)", "gdppc24"="2024pr")|> 
  select(DPTO, DEPARTAMENTOS, gdppc24) |> 
  filter(!DPTO %in% c(NA,"88")) |> 
  arrange(DPTO) |> 
  mutate(gdppc24_2 = gdppc24/1000000)

coldep_data<-merge(coldep, data, by="DPTO")

ggplot(coldep_data) +
  geom_sf(aes(fill = gdppc24_2), color = "#ffffff", size = 0.05) +
  labs(fill ="PIB percapita 2024") +
  geom_sf_text(aes(label = NOMBRE_DPT), size=2.3) +
  scale_fill_viridis_c(direction = -1) +
  theme_void() +
  theme(legend.position = "right",
        legend.text=element_text(size=10)) +
  canvas(9.5,9, units = 'in')

ggsave("f2.png", width = 9.5, height = 9, dpi = 300, bg="white")
