# A partir de los datos de la encuesta de salud

# 1. obtenga un resumen númerico completo del colesterol

# Load dataset
# Set working directory
setwd("/home/nicoluarte/Downloads/")
# Import dataset
ENS <- data.frame(read.csv2("ENS.csv")) 
# Create summary function
diplomado_summary <- function(column_vector)
{
  library(e1071)
  # Indicadores descriptivos
  #Rango, diferencia entre el máximo y el mínimo
  el_Rango <- abs(max(column_vector) - min(column_vector))
  # Rango intercuartil, es la distancia dónde se concentra el 50% central
  # diferencia entre el tercer y primer cuartil
  #Calculate quantile
  tmp <- quantile(column_vector)
  intr_qrtl_range <- tmp["75%"] - tmp["25%"]
  #Varianza, es un indicador sin interpretación, que sirve para calcular la desviación estándar
  # que corresponde a la dispersión d edatos respecto a la media
  varianza <- var(column_vector)
  # Coeficiente de variación, es adimensional, y responde a la relació de desviación vs media
  desv_std <- sd(column_vector)
  media <- mean(column_vector)
  coef_var <- desv_std - abs(media)
  #Kurtosis
  la_Kurtosis <- kurtosis(column_vector)
  # Asimetría
  la_asimetria <- skewness(column_vector)
  # Create named vector
  stat_obj <- c(el_Rango, intr_qrtl_range, varianza, coef_var, la_Kurtosis, la_asimetria)
  names(stat_obj) <- c("Rango", "RIC", "Varianza", "Coef. Variación", "Kurtosis", "Asimetría")
  return(stat_obj)
}

# Resumen colesterol
resumen_colesterol <- diplomado_summary(ENS$COLESTEROL)
# Resumen peso
resumen_peso <- diplomado_summary(ENS$PESO)
# resumen HDL hembra
resumen_HDL_F <- diplomado_summary(ENS$HDL[ENS$HDL & ENS$SEXO == "Femenino"])
# resumen HDL macho
resumen_HDL_M <- diplomado_summary(ENS$HDL[ENS$HDL & ENS$SEXO == "Masculino"])

# Busque la correlación entre la TALLA y el PESO, concluya
# Plot to inspect data
scatter.smooth(ENS$TALLA, ENS$PESO, xlab = "Talla",  ylab = "Peso")

# Busque la correlación entre la TALLA y el PESO
library(corrplot)
library(RColorBrewer)
library(sjPlot)
library(grDevices)
library(dplyr)
TPEC <- matrix(data = c(ENS$TALLA, ENS$PESO, ENS$EDAD, ENS$COLESTEROL), nrow= 350, ncol = 4)
colnames(TPEC) <- c("TALA", "PESO", "EDAD", "COLESTEROL")
whiteblack <- c("white", "black")
# corr confidence at .95
res1 <- cor.mtest(TPEC, conf.level = .95)
corrplot(cor(TPEC), p.mat = res1$p, method = "circle", bg = "white")
# gráfique el colesterol y comente
# ...
# grafique el perimetro del cuello, segmentando por personas que realiza deporte y las que no realizan deporte
# deporte si/no como 1, 2
deporte <- unclass(ENS$DEPORTE)
# crear matriz deporte diametro del cuello
cuell_dep <- matrix(c(deporte, ENS$CUELLO), ncol = 2, nrow = length(ENS$CUELLO))
boxplot.matrix(cuell_dep, names=c("DEPORTE", "! DEPORTE"))


