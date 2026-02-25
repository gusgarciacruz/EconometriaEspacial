# ANÁLSIS DE CONVERGENCIA DEL PIB PERCAPITA EN COLOMBIA
# PAPER: Vicente Royuela & Gustavo A. García (2015) Economic and Social Convergence in Colombia
# Regional Studies, 49(2): 219-239
# http://www.tandfonline.com/doi/abs/10.1080/00343404.2012.762086#.VdS8oLJ_Oko

library(tidyverse); library(KernSmooth); library(rgl); library(ggalt); library(reshape2)
library(ggview)

setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometría espacial/R/L4")

data<-read.csv("data1.csv", header = TRUE, sep = ",", dec=".") |> 
  mutate(deptos = str_sub(Dept, start = 1, end = 3)) |> 
  mutate(deptos = case_when(deptos=="C\xf3r" ~ "Cor",
                            deptos=="La " ~ "La Gua",
                            TRUE ~ deptos))

# Las variables relativas se calculan como: ln(GDP_pc1975_i) - ln(mean(GDP_pc1975))
  
# Estadísticas descriptivas
colnames(data)
nrow(data)
ncol(data)

summary(data)
summary(data$GDP_pc2005)
sd(data$GDP_pc2005)

ggplot(data = data, aes(x = LGDP_pcr2005, y = LGDP_pcr1975, label = deptos)) +
  geom_point(color="red") + # Adds the points to the plot
  geom_text(vjust = -0.5, hjust = 0.5, cex=3.5) + # Adds the labels
  #xlim(-0.5, 1.5) + ylim(-0.5, 1.5) +
  labs(x = "Log GDP pc relative 2005", y = "Log GDP pc relative 1975") + # Labels for axe()
  geom_hline(yintercept = 0, color = "gray60", size = 0.5) +
  geom_vline(xintercept = 0, color = "gray60", size = 0.5)
  
# Ecuación de convergencia
tc_gdp <- log(data$GDP_pc2005/data$GDP_pc1975)/31
summary(tc_gdp)
LGDP_pc1975<-log(data$GDP_pc1975)
model1<-lm(tc_gdp~LGDP_pc1975, data=data)
summary(model1)
summary(model1)$coefficients
vc <- log(1-(31*summary(model1)$coefficients[2, 1]))/31
vc
halflife <- log(2)/vc
halflife

# Kernels estocásticos
x<-data.frame(data[,"LGDP_pcr2005"],data[,"LGDP_pcr1975"])

# Univariate Kernel
help(kde) 
h1 <- dpik(x[,1])
h2 <- dpik(x[,2])
fhat1 <- bkde(x[,1], bandwidth=h1)
fhat2 <- bkde(x[,2], bandwidth=h2)

df_plot <- data.frame(
  val = c(fhat1$x, fhat2$x),
  dens = c(fhat1$y, fhat2$y),
  anio = rep(c("2005", "1975"), each = length(fhat1$x)))

ggplot(df_plot, aes(x = val, y = dens, linetype = anio)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(limits = c(-2, 2)) +
  scale_y_continuous(limits = c(0, 0.9)) +
  scale_linetype_manual(values = c("2005" = "solid", "1975" = "dashed")) +
  labs(x = "Log of relative GDP pc", y = "Density function", linetype = "") +
  theme_classic() +
  theme(legend.position = c(0.85, 0.85),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.5, "cm")) + 
  canvas(4, 4)

# Bivariate Kernel
est <- bkde2D(x, c(h1, h2))

dimnames(est$fhat) <- list(est$x1, est$x2)
df_contour <- melt(est$fhat)
colnames(df_contour) <- c("x", "y", "z")

ggplot() +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = 0, color = "gray70") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_contour(data = df_contour, aes(x = x, y = y, z = z),
               breaks = c(0.1, 0.25, 0.5, 0.75, 0.9), 
               color = "black") +
  annotate("text", x = x[,1], y = x[,2], label = data$deptos, 
           vjust = -0.7, size = 3) +
  scale_x_continuous(limits = c(-1.5, 1.5), 
                     breaks = seq(-1.5, 1.5, 0.5),
                     expand = expansion(mult = 0.05)) +
  scale_y_continuous(limits = c(-1.5, 1.5), 
                     breaks = seq(-1.5, 1.5, 0.5),
                     expand = expansion(mult = 0.05)) +
  labs(x = "Log relative GDP pc 2005", 
       y = "Log relative GDP pc 1975") +
  theme_bw() + # Fondo blanco con bordes grises
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.ticks = element_line(colour = "black")) + 
  canvas(6, 4)
  
ggsave("g7.png", width = 6, height = 4, units="in", bg="white")

# Trivariate Kernel
help(persp)
png("g8.png", width = 800, height = 800, res = 120)
persp(est$fhat, theta = -30, phi = 35, col="gray60", ticktype = "detailed",  
      ltheta = 0, shade = 0.75, border = NA, cex.axis=0.7, cex.lab=0.8,
      xlab = "Log relative GDP pc 2005", ylab = "Log relative GDP pc 1975", zlab = "Density") 
dev.off()

# Interactivo
options(rgl.useNULL = TRUE)
clear3d()

persp3d(x = est$x1,           
        y = est$x2,           
        z = est$fhat,         
        col = "gray60", 
        smooth = TRUE,         
        lit = TRUE,            
        xlab = "Log relative GDP pc 2005", 
        ylab = "Log relative GDP pc 1975", 
        zlab = "Density")

mi_widget <- rglwidget()
mi_widget
