# ESDA
library(spdep); library(sf); library(ggview); library(tidyverse); library(ape)
library(summarytools); library(ggpubr); library(rgeoda); library(geostan);
library(sfdep)

setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometría espacial/R/L5")

# Se usaran datos del censo del 2000 de Manhattan en New York City: variables socioeconómicas de 297
# census tracts

# Cargando el shapefile
ny<-st_read("NewYork.shp")

ggplot() + 
  geom_sf(data=ny, colour = "gray95", fill = "gray90") +
  theme_void() +
  canvas(3,5, units = "in")

# Se elemina la isla
ny<-st_read("NewYork.shp") |> 
  filter(TRACT != 1)

ggplot() + 
  geom_sf(data=ny, colour = "gray95", fill = "gray90") +
  theme_void() +
  canvas(3,5, units = "in")

# Construyendo la matriz de pesos espaciales 
# Tipo Queen
nb <- poly2nb(ny,queen=T)
nb
We <- nb2listw(nb, style="W")
names(We)
We$weights

# Opción que queda en forma de matriz
W2 <- shape2mat(ny, method = 'queen')
W2e <- row_standardize(W2) |> 
  as.matrix()

# Graficando la contiguidad
cnt <- st_centroid(ny, of_largest_polygon = T) # construyendo los centroides
centroides <- cnt  |>  st_coordinates()

line_nb <- nb2lines(nb, coords = centroides, proj4string = 4326, as_sf = T)

ggplot() +
  geom_sf(data=ny, colour = "gray95", fill = "gray90") +
  geom_sf(data=cnt, color="red")  +
  geom_sf(data = line_nb, fill = "grey40", size=.4) +
  theme_void()

# Contrastes globales de autocorrelación espacial
# I de Moran
moran.test(ny$T0P_UEMP, We)
moran.mc(ny$T0P_UEMP, listw=We, nsim=999)
Moran.I(ny$T0P_UEMP, W2e)

# C de Geary
# Geary's C is a measure of spatial dissimilarity. Values range from 0 to 2, where: 
# Values less than 1 indicate positive spatial autocorrelation (similar values cluster together).
# A value of 1 indicates no spatial autocorrelation (random distribution).
# Values greater than 1 suggest negative spatial autocorrelation (dissimilar values cluster together). 
# This interpretation is the inverse of the Moran's I statistic.
geary.test(ny$T0P_UEMP, We)

# G(d) de Getis y Ord
# globalG.test computes a global test for spatial autocorrelation using a Monte Carlo simulation approach (simulated spatial datasets 
# that have the same spatial structure as the original data but are randomly permuted). It tests the null hypothesis of no spatial 
# autocorrelation against the alternative hypothesis of positive spatial autocorrelation.

# Binary weighting assigns a weight of 1 to all neighboring features and a weight of 0 to all other features
# I used binary weighting to assess the overall spatial distribution. Binary weighting assigns a weight of 1 to all neighboring 
# features (ignoring relative size or extent) and a weight of 0 to all other features.
W1 <- nb2listw(nb, style="B")
globalG.test(ny$T0P_UEMP, W1)

# The output shows a standard deviate of 5.1784, which indicates that the observed clustering of unemployment is 5.1784 standard deviations 
# away from what would be expected under the null hypothesis of no clustering. This value is associated with a p-value of 1.119e-07, so observed
# clustering is statistically significant at the 0.05 level. The alternative hypothesis is “greater,” which means that the analysis is looking for 
# clusters of high unemployment values.
# Overall, the output suggests that there is statistically significant clustering of high unemployment values.

# Scatterplot de Moran
# Opción 1
# Forma sin editar
mp <- moran.plot(as.vector(scale(ny$T0P_UEMP)), We,pch=19)

moran.plot(as.vector(scale(ny$T0P_UEMP)), We, labels=FALSE, quiet=FALSE,
xlim=c(-2,8), ylim=c(-2,8), pch=19, xlab="% unemployment", 
ylab="Spatial lag % unemployment")
title("Moran scatterplot")
text(x=6.5, y=-1,"Moran's I=0.3087",cex=.8)
text(x=6.5, y=-1.8,"P-value=0.000",cex=.8)

# Opción 2
ny <- ny |> 
  mutate(st_T0P_UEMP     = scale(ny$T0P_UEMP),
         lag_st_T0P_UEMP = lag.listw(We, st_T0P_UEMP))

ggplot(ny, aes(x=st_T0P_UEMP, y=lag_st_T0P_UEMP)) + 
  geom_point(shape=1, size=1) + 
  geom_smooth(formula=y ~ x, method="lm", se=F) + 
  geom_hline(yintercept=0, lty=2) + 
  geom_vline(xintercept=0, lty=2) + 
  theme_minimal() +
  geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=7, size=1) +
  geom_text(data=mp[mp$is_inf,], aes(x=x, y=wx, label=labels, vjust=1), size=2) +
  xlim(-1.5,7) + ylim(-4,3) +
  xlab("% unemployment") + 
  ylab("Spatial lag % unemployment") + 
  theme(axis.title.y = element_text(size = rel(.6)),
        axis.title.x = element_text(size = rel(.6))) +
  annotate("text", x = 2, y = -3, label = "Moran's I = 0.3087", size=2) +
  annotate("text", x=2, y=-3.3, label="P-value = 0.000", size=2) +
  canvas(units = "in", 7, 4)

ggsave("scatt_imoran.png", 
       width = 7, height = 4, units = "in", dpi = 300, bg="white")

# Opción 3
ny <- ny |> 
  mutate(lisa_group = case_when(st_T0P_UEMP>=0  & lag_st_T0P_UEMP>=0 ~ "HH",
                                st_T0P_UEMP<0   & lag_st_T0P_UEMP<0  ~ "LL",
                                st_T0P_UEMP>=0  & lag_st_T0P_UEMP<0  ~ "HL",
                                st_T0P_UEMP<0 & lag_st_T0P_UEMP>=0   ~ "LH"))
freq(ny$lisa_group)

ggplot(ny,aes(st_T0P_UEMP, lag_st_T0P_UEMP)) +
  geom_smooth(formula=y ~ x, method="lm", se=F, size = .5) +
  geom_point(aes(st_T0P_UEMP, lag_st_T0P_UEMP, color = lisa_group), size=.5) +
  #geom_text(aes(st_T0P_UEMP, lag_st_T0P_UEMP, label=F, vjust=1.8, color = lisa_group), size=1) +
  geom_vline(aes(xintercept = 0), lty = 2, alpha = 1/3) +
  geom_hline(aes(yintercept = 0), lty = 2, alpha = 1/3) + 
  labs(y = "Spatial lag % unemployment",
       x = "% unemployment", color=NULL) +
  theme_minimal() +
  scale_color_manual(values=c("red", "lightpink", "skyblue2","blue")) +
  theme(axis.title.y = element_text(size = rel(.5)),
        axis.title.x = element_text(size = rel(.5)),
        legend.text = element_text(size = 5),
        legend.spacing.x = unit(.01, 'cm'),
        axis.text=element_text(size=4),
        legend.key = element_rect(size = 2, color = 'white'),
        legend.key.size = unit(.5, 'lines')) +
  annotate("text", x = 2, y = -3, label = "Moran's I = 0.3087", size=1.5) +
  annotate("text", x=2, y=-3.2, label="P-value = 0.000", size=1.5)+
  canvas(units = "in", 4, 3)

ggsave("scatt_imoran2.png", 
       width = 4, height = 3, units = "in", dpi = 300, bg="white")

# Contrastes locales de autocorrelación espacial
# Local I de Moran
# A positive value for Ii indicates that the unit is surrounded by units with similar values
lmoran <- localmoran(ny$T0P_UEMP, We)
View(lmoran)
summary(lmoran)

# Plot local Moran
moran.map <- cbind(ny, lmoran)
names(moran.map)
View(moran.map[,c("POLYID","STATE", "COUNTY", "TRACT",
                  "T0P_UEMP","st_T0P_UEMP","lag_st_T0P_UEMP","Ii", "Pr.z....E.Ii..")])

uem_map <- ggplot(moran.map) + 
  geom_sf(aes(fill = T0P_UEMP), color = "gray35", size = 0.05) +
  #geom_sf_text(aes(label = STUSPS), size=1.5, colour = "black") +
  scale_fill_viridis_c(direction = -1) +
  labs(fill="% unemployment") +
  theme_void() +
  theme(legend.position = c(1,.25),
        legend.key.size = unit(0.4,"cm"),
        legend.text=element_text(size=6),
        legend.title=element_text(size=7)) 

uem_map + canvas(3,5, units = "in")

summary(moran.map$Ii)

moran.map <- moran.map |> 
  mutate(posneg = case_when(Ii<0~"(-)",
                            Ii>=0~"(+)"))

Ii_uem <- ggplot(moran.map) + 
  geom_sf(aes(fill = posneg), color = "gray35", size = 0.05) +
  #geom_sf_text(aes(label = STUSPS), size=2, colour = "black") +
  scale_fill_manual(values = c("white","skyblue")) +
  labs(fill="Local Moran stat") +
  theme_void() +
  theme(legend.position = c(.85,.2),
        legend.key.size = unit(0.4,"cm"),
        legend.text=element_text(size=7),
        legend.title=element_text(size=7)) 
Ii_uem + canvas(3,5, units = "in")

ggarrange(uem_map, Ii_uem, 
          ncol = 2, nrow = 1) +
  canvas(units = "in", width = 5, height = 3)

# Plot LISA clusters
# Construyendo los cuadrantes high-high, low-low, high-low, low-high quadrant y no signficante
moran.map <- moran.map |>
  rename(pval = "Pr.z....E.Ii..") |> 
  mutate(quad_sig = case_when(st_T0P_UEMP >= 0 & lag_st_T0P_UEMP >= 0 & pval <= 0.05  ~ "high-high",
                              st_T0P_UEMP <= 0 & lag_st_T0P_UEMP <= 0 & pval <= 0.05  ~ "low-low",
                              st_T0P_UEMP >= 0 & lag_st_T0P_UEMP <= 0 & pval <= 0.05  ~ "high-low",
                              st_T0P_UEMP <= 0 & lag_st_T0P_UEMP >= 0 & pval <= 0.05  ~ "low-high",
                              pval > 0.05 ~ "Not signif."))

freq(moran.map$quad_sig)

Ii_sig_uem <-ggplot(moran.map) + 
  geom_sf(aes(fill = quad_sig), color = "gray50", size = 0.05) +
  #geom_sf_text(aes(label = STUSPS), size=2, colour = "black") +
  scale_fill_manual(values = c("red", "skyblue2", "blue","white"),
                    labels = c("High-High", "Low-High", "Low-Low", "Not Signif.")) +
  labs(fill="LISA") +
  theme_void() +
  theme(legend.position = c(.85,.2),
        legend.key.size = unit(0.4,"cm"),
        legend.text=element_text(size=7),
        legend.title=element_text(size=7))

Ii_sig_uem + canvas(3,5, units = "in")

ggarrange(uem_map, Ii_uem, Ii_sig_uem, 
          ncol = 3, nrow = 1) +
  canvas(units = "in", width = 8, height = 4, dpi = 300, bg="white")

# Otra forma de hacer el LISA con rgeoda
# https://geodacenter.github.io/rgeoda/
#knn_w <- rgeoda::knn_weights(us_merge, k=6) # nearest neighborhs weights
w <- rgeoda::queen_weights(ny, order = 1)
lisa <- local_moran(w, ny["T0P_UEMP"], # select only one column with data
                    permutations = 999,
                    permutation_method = "complete",
                    significance_cutoff = 0.05,
                    cpu_threads = 2,
                    seed = 123456789)
ny$cluster = as.factor(lisa$GetClusterIndicators())
levels(ny$cluster) = lisa$GetLabels()
ny %>% freq(cluster)

ny$cluster = factor(ny$cluster,
                          levels = c("High-High", 
                                     "High-Low",
                                     "Low-High",
                                     "Low-Low",
                                     "Not significant")) # convert to factor

ggplot() + geom_sf(data=ny, aes(fill = cluster), color=NA, show.legend=TRUE) +
  scale_fill_manual(values = c("red", "pink", "lightblue", "darkblue", "grey95"), drop=F) + 
  labs(fill = "LISA") +
  guides(fill=guide_legend(title.position = "top")) +
  theme(panel.background = element_rect(fill = "white"), #fondo del gr?fico
        legend.position = c(0.75, 0.2), #ubicacion de leyenda, dentro del gráfico
        legend.key.size = unit(0.2, "cm"), #alto de rectangulo de referencia
        legend.key.width = unit(0.2,"cm"), #ancho de rectangulo de referencia
        legend.text=element_text(size=4), #tamaño de texto de leyenda
        legend.background = element_rect(fill=NA), #background de la leyenda
        legend.title=element_text(size=6), #tamaño título leyenda
        axis.text = element_blank(), #texto eje X e Y
        axis.ticks = element_blank()) +  #eje X e Y
   canvas(3,5, units = "in")

ggsave("Ii_sig_rap.png", 
       width = 3, height = 5, units = "in", dpi = 300, bg="white")

# Gi(d) y G*i(d) de Getis y Ord
# Identify neighbors, create weights, calculate spatial lag
ny_nbs <- ny |> 
          mutate(nb = st_contiguity(geometry),        # neighbors share border/vertex
                 wt = st_weights(nb))                 # row-standardized weights
     
# The Gi is the ratio of the spatial lag of a feature to the sum of the feature’s 
# values for its neighbors. A positive Gi value indicates that a feature and 
# its neighbors have high values, while a negative Gi value indicates that they 
# have low values. The magnitude of the Gi value indicates the strength of 
# the clustering.

# Calculate the Gi
ny_hot_spots <- ny_nbs |> 
                mutate(Gi = local_g_perm(T0P_UEMP, nb, wt, nsim = 999)) |> # nsim = number of Monte Carlo simulations (999 is default)
  # The new 'Gi' column itself contains a dataframe 
  # We can't work with that, so we need to 'unnest' it
  unnest(Gi) 

ny_hot_spots |> 
  ggplot((aes(fill = gi))) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_gradient2() +  # makes the value 0 (random) be the middle
  theme_void() +
  labs(fill = "Gi") +
  theme(legend.position = c(0.8, 0.2), #ubicacion de leyenda, dentro del gráfico
        legend.text=element_text(size=7), #tamaño de texto de leyenda
        legend.title=element_text(size=8) #tamaño título leyenda
  ) +
  canvas(3,5, units = "in")

# But is it statistically significant? We will consider p-values in the next step.

ny_hot_spots |> 
  # with the columns 'gi' and 'p_folded_sim"
  # 'p_folded_sim' is the p-value of a folded permutation test
  select(gi, p_folded_sim) |> 
  mutate(
    # Add a new column called "classification"
    classification = case_when(
      # Classify based on the following criteria:
      gi > 0 & p_folded_sim <= 0.01 ~ "Very hot",
      gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
      gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat hot",
      gi < 0 & p_folded_sim <= 0.01 ~ "Very cold",
      gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
      gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat cold",
      TRUE ~ "Insignificant"
    ),
    # Convert 'classification' into a factor for easier plotting
    classification = factor(
      classification,
      levels = c("Very hot", "Hot", "Somewhat hot",
                 "Insignificant",
                 "Somewhat cold", "Cold", "Very cold"))) |> 
  # Visualize the results with ggplot2
  ggplot(aes(fill = classification)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  theme_void() +
  labs(fill = "Hot spot classification") +
  theme(legend.position = c(0.8, 0.2), #ubicacion de leyenda, dentro del gráfico
        legend.key.size = unit(0.2, "cm"), #alto de rectangulo de referencia
        legend.key.width = unit(0.2,"cm"), #ancho de rectangulo de referencia
        legend.text=element_text(size=5), #tamaño de texto de leyenda
        legend.title=element_text(size=6) #tamaño título leyenda
        ) +
  canvas(3,5, units = "in")
