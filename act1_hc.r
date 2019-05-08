# get working directory
getwd()
# set dataset filepath
setwd("/home/nicoluarte/Downloads/")
# add library for xlsx
library(readxl)
library(stringr)
# read data 
encuesta <- data.frame(read_excel("OoOXDe9ooHVdVPNencuesta.xlsx", sheet = 1))
nombres <- data.frame(read_excel("OoOXDe9ooHVdVPNencuesta.xlsx", sheet = 2))
# put col labels
# colnames(encuesta) <- c(nombres[,2])
# remove word "region" in region column
# create string with regions
# get last word
rStr = c(word(encuesta$P3, -1))
# replace with created vector
encuesta[,3] <- rStr
# Only males form valparaiso
male_val = subset.data.frame(encuesta, (encuesta$P9=="Hombre") & (encuesta$P3=="Valparaíso"))
# Age mean males valparaiso victima delito
vic_male_vale = subset.data.frame(male_val, male_val$P64=="Si")
# Sum casados male valpo
amnt_casados = sum(c(male_val$P13 == "Casado/a"), na.rm = TRUE)
# mujeres coquimbo dinero en seguridad
fem_sec_din = subset.data.frame(encuesta, (encuesta$P9=="Mujer") & (encuesta$P154!="No gastó dinero en medidas de seguridad") & (encuesta$P154!="No sabe"))
