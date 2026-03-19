library(tidyverse); library(lmtest); library(sp); library(spdep); library(RColorBrewer)
library(ggview); library(spatialreg)

# Se analizan los determinantes de la tasa de vivienda propia para NYC a nivel de Census track
setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometría espacial/R/L6")

nyc<-st_read("nyc2000.shp")

ggplot() + 
  geom_sf(data=nyc, colour = "gray95", fill = "gray90") +
  theme_void() +
  canvas(3,3, units = "in")

# Estadísticas de la tasa de vivienda propia (T0P_OWN) 
hist(nyc$T0P_OWN)
summary(nyc$T0P_OWN)

# Mapeando la tasa de casa propia
ggplot(nyc) +
  geom_sf(aes(fill = T0P_OWN), color = "gray50", size = 0.05)  +
  scale_fill_viridis_c(direction = -1) + labs(fill="Vivienda propia (%)") +
  theme_void() +
  theme(legend.position = c(.9,.15),
        legend.title = element_text(size = 4.5),
        legend.key.size = unit(0.15,"cm"),
        legend.text=element_text(size=4),
        legend.spacing = unit(.1,"cm"),
        legend.spacing.x = unit(.04, 'cm')) +
  canvas(3,3, units = 'in')

# Estimación MCO
ols<-lm(T0P_OWN~PCTNHW+PCTNHB+T0P_COLL+T0P_UEMP+T0P_FOR+T0_MINC+T0P_POOR, data=nyc)
summary(ols)

# Mapeando los residuales para detectar algún patrón espacial
nyc$resid<-residuals(ols)
summary(nyc$resid)

ggplot(nyc) +
  geom_sf(aes(fill = cut_number(resid, n = 5)), color = "gray50", size = 0.05) +
  scale_fill_viridis_d(direction = -1, option = "viridis", name = "Residuales OLS") +
  theme_void() +
  theme(legend.position = c(.9,.15),
        legend.title = element_text(size = 4.5),
        legend.key.size = unit(0.15,"cm"),
        legend.text=element_text(size=4),
        legend.spacing = unit(.1,"cm"),
        legend.spacing.x = unit(.04, 'cm')) +
  canvas(3,3, units = 'in')

# Create a k=4 nearest neighbor set
coords <- st_centroid(st_geometry(nyc), of_largest_polygon=TRUE)
plot(coords)
nyc.nb4 <- knearneigh(coords, k=4)
nyc.knn4 <- knn2nb(nyc.nb4)
We <- nb2listw(nyc.knn4, style = "W")
names(We)
We$weights

# I de Moran Global
lm.morantest(ols, listw = We)

# Tests LM
lm.RStests(ols, listw=We, test=c("LMerr", "LMlag",
          "RLMerr", "RLMlag", "SARMA"))

# Modelo SAR
sar<-lagsarlm(T0P_OWN~PCTNHW+PCTNHB+T0P_COLL+T0P_UEMP+T0P_FOR+T0_MINC+T0P_POOR, 
              data=nyc, listw=We, type="lag", method="MC")
summary(sar, Nagelkerke=T)

# Modelo SEM
sem<-errorsarlm(T0P_OWN~PCTNHW+PCTNHB+T0P_COLL+T0P_UEMP+T0P_FOR+T0_MINC+T0P_POOR,
                data=nyc, listw=We, etype="error", method="MC")
summary(sem, Nagelkerke=T)

# Modelo Spatial Durbin model
sdm<-lagsarlm(T0P_OWN~PCTNHW+PCTNHB+T0P_COLL+T0P_UEMP+T0P_FOR+T0_MINC+T0P_POOR,
              data=nyc, listw=We, type="mixed", method="MC")
summary(sdm, Nagelkerke=T)

# Modelo Spatial Durbin error model
sdem<-errorsarlm(T0P_OWN~PCTNHW+PCTNHB+T0P_COLL+T0P_UEMP+T0P_FOR+T0_MINC+T0P_POOR,
                 data=nyc, listw=We, etype="emixed", method="MC")
summary(sdem, Nagelkerke=T)

# Modelo SAC-SARAR-SARMA
sac<-sacsarlm(T0P_OWN~PCTNHW+PCTNHB+T0P_COLL+T0P_UEMP+T0P_FOR+T0_MINC+T0P_POOR,
              data=nyc, listw=We, type="sac", method="MC")
summary(sac, Nagelkerke=T)

# Modelo SLX
# Opción 1
nyc$wPCTNHW<-lag.listw(We,nyc$PCTNHW)
nyc$wPCTNHB<-lag.listw(We,nyc$PCTNHB)
nyc$wT0P_COLL<-lag.listw(We,nyc$T0P_COLL)
nyc$wT0P_UEMP<-lag.listw(We,nyc$T0P_UEMP)
nyc$wT0P_FOR<-lag.listw(We,nyc$T0P_FOR)
nyc$wT0_MINC<-lag.listw(We,nyc$T0_MINC)
nyc$wT0P_POOR<-lag.listw(We,nyc$T0P_POOR)

slx<-lm(T0P_OWN~PCTNHW+PCTNHB+T0P_COLL+T0P_UEMP+T0P_FOR+T0_MINC+T0P_POOR+
                wPCTNHW+wPCTNHB+wT0P_COLL+wT0P_UEMP+wT0P_FOR+wT0_MINC+wT0P_POOR,
        data=nyc)
summary(slx, Nagelkerke=T)

# Opción 2
slx2<-lmSLX(T0P_OWN~PCTNHW+PCTNHB+T0P_COLL+T0P_UEMP+T0P_FOR+T0_MINC+T0P_POOR,
            data = nyc, listw=We)
summary(slx2)

# Calculando el AIC y comparandolo entre todos los modelos
AICs<-c(AIC(ols),AIC(sar), AIC(sem), AIC(sdm),
        AIC(sdem), AIC(sac), AIC(slx))
plot(AICs, type="l", lwd=1.5, xaxt="n", xlab="")
axis(1, at=1:7,labels=F) #7= number of models
labels<-c("OLS", "sar","sem", "sdm","sdem", "sac", "slx")
text(1:7, par("usr")[3]-.25, srt=45, adj=1, labels=labels, xpd=T)
mtext(side=1, text="Model Specification", line=3)
symbols(x= which.min(AICs), y=AICs[which.min(AICs)], circles=1, fg=2,lwd=2,add=T)

knitr::kable(data.frame(Models=labels, AIC=round(AICs, 2)))

# Test LR para comparar entre modelos
anova(sar, sdm)
anova(sem, sdm)
anova(sac, sdm)

# Calculo de los efectos directos e indirectos
# Se sigue la estrategía de convertir la matriz de pesos espaciales en una
# matrix "sparse" (W usulamente es "sparse", no densa, significando que 
# tiene una gran proporción de ceros) y se potencia utilizando la función 
# trW(), como los sugieren Lesage y Pace (2009, Cap 4). De acuerdo a estos autores,
# la téncia de matrices "sparse" facilita los cálculos computacionales
W <- as(We, "CsparseMatrix") 
trMC <- trW(W, type="MC")
im<-impacts(sdm, tr=trMC, R=100)
summary(im,  zstats=T, short=T)

# Interpretación
# Basados sobre el impacto total de -1.0128 para el % de personas con college, 
# se puede concluir que un 10% de incremento en el % de personas con college 
# genera una disminución de 10.128% en el % de casas propias.
# Alredero del 22% de este impacto viene del efecto directo de -0.2191, y 78%
# del indirecto o spillover espacial basado sobre la estimaci?n de -0.7937

# Particionamiento espacial de los impactos estimados
# Los resultados de los efectos directos e indirectos anteriores no responden
# a la pregunta de cuán importantes son los vecinos inmediatos para la 
# tasa de propietarios
# Se puede particionar espacialmente estos impactos para ilustrar la natureleza
# de su influencia cuando nos movemos de un vecino cercano a más lejanos
# Esto puede ser de interés en aplicaciones donde el grado espacial de los spillovers
# es objeto de inferencia
# A continuación se presentan los efectos marginales asociados con matrices W de 
# 0 a un orden de 5
im2<-impacts(sdm, tr=trMC, R=100, Q=5)
summary(im2,  zstats=T, reportQ=T, short=T)

# Se observa que el efecto directo e indirecto muestran el decaimiento esperado
# a mayor orden de W
# Se puede inferir que un incremento de 10% en el % de personas con college
# podría tener un efecto indirecto o efecto spillover espacial correspondiente a
# una disminuci?n de 2.9% en el % de casas propias del vecino más próximo
# (vecino de primero orden), disminución de 2.1% en el % de casas propias
# del vecino de segundo orden, 1.2% de disminución en el vecino de tercer
# orden, y así sucesivamente.
