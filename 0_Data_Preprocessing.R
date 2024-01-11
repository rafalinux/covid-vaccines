#!/usr/bin/Rscript

# 16 July 2023
# Processing the file from the Spanish Ministry of Health: patients hospitalized with COVID-19 diagnosis from 2020 to 2021
# 
#

######################################
#                                    #
# SETTING WORKING DIRECTORY          #
#                                    #
######################################
setwd("~/Python_Projects/COVID_CMBD/Articulo_Differences/")

############################################
#                                          #
# LOADING DATA and PROCESSING COLUMNS      #
#                                          #
############################################
# Please, note that the .CSV file cannot be included in this repository, due to the contract with the
# Spanish Ministry of Health
pacientes <- read.table("pacientes_2020-2021.csv", header=TRUE, sep=",")

# Changing date format
pacientes$Fecha.de.Ingreso <- gsub('.{5}$', '', pacientes$Fecha.de.Ingreso) #Removing hours and minutes
pacientes$Fecha.de.Fin.Contacto <- gsub('.{5}$', '', pacientes$Fecha.de.Fin.Contacto) #Removing hours and minutes

pacientes$Fecha.de.Ingreso <- as.POSIXct(strptime(pacientes$Fecha.de.Ingreso,"%d%m%Y"))
pacientes$Fecha.de.Fin.Contacto <- as.POSIXct(strptime(pacientes$Fecha.de.Fin.Contacto,"%d%m%Y"))

# Converting variables into factors
pacientes$Sexo <- as.factor(pacientes$Sexo)
pacientes$Ingreso.en.UCI <- as.factor(pacientes$Ingreso.en.UCI)
pacientes$Tipo.Alta <- as.factor(pacientes$Tipo.Alta)
pacientes$Año <- as.factor(pacientes$Año)
pacientes$Comunidad.Autónoma <- as.factor(pacientes$Comunidad.Autónoma)

# Some patients are from 2019, they are outliers:
pacientes <- pacientes[pacientes[["Fecha.de.Ingreso"]] >= "2020-01-01", ]

# Sexo
# 1 Varón
# 2 Mujer
# 9 No especificado 
########
# Tipo de alta
# 1 Domicilio
# 2 Traslado a otro hospital
# 3 Alta voluntaria y/o fuga
# 4 Éxitus
# 5 Traslado a centro sociosanitario
# 8 Otros
# 9 Desconocido 
########
# UCI:
# 1 Sí
# 2 No 
#######
# Comunidad:
# 1 ANDALUCÍA
# 2 ARAGÓN
# 3 ASTURIAS (PRINCIPADO DE)
# 4 BALEARS (ILLES)
# 5 CANARIAS
# 6 CANTABRIA
# 7 CASTILLA Y LEÓN
# 8 CASTILLA#LA MANCHA
# 9 CATALUÑA
# 10 COMUNIDAD VALENCIANA
# 11 EXTREMADURA
# 12 GALICIA
# 13 MADRID (COMUNIDAD DE)
# 14 MURCIA (REGION DE)
# 15 NAVARRA (COMUNIDAD FORAL DE)
# 16 PAIS VASCO
# 17 RIOJA (LA)
# 18 CEUTA
# 19 MELILLA

# Dates in figures are now in English, not in Spanish
Sys.setlocale("LC_TIME", "en_GB.UTF-8") 

##############################################################
#                                                            #
# ADDING COMORBIDITIES                                       #
#                                                            #
##############################################################

source("comorbidities.R")

##############################################################
#                                                            #
# DEFINING EPIDEMIC WAVES, ADAPTED TO SPAIN                  #
#                                                            #
##############################################################

funcion_olas <- function(x,y){pacientes[pacientes$Fecha.de.Ingreso >= x & pacientes$Fecha.de.Ingreso <= y,]}

# In two years we had 6 waves, although the sixth wave did not finished until 2022
df_primera_ola <- funcion_olas("2020-01-01","2020-06-21")
df_segunda_ola <- funcion_olas("2020-06-22","2020-12-06") 
df_tercera_ola <- funcion_olas("2020-12-07","2021-03-14")
df_cuarta_ola <- funcion_olas("2021-03-15","2021-06-19")
df_quinta_ola <- funcion_olas("2021-06-20","2021-10-13")
df_sexta_ola <- funcion_olas("2021-10-14","2021-12-31")

# "ola" in Spanish is "wave" in English
pacientes$ola <- 0
pacientes$ola[pacientes$Fecha.de.Ingreso >= "2020-01-01" & pacientes$Fecha.de.Ingreso <= "2020-06-21"] <- 1
pacientes$ola[pacientes$Fecha.de.Ingreso >= "2020-06-22" & pacientes$Fecha.de.Ingreso <= "2020-11-06"] <- 2
pacientes$ola[pacientes$Fecha.de.Ingreso >= "2020-12-07" & pacientes$Fecha.de.Ingreso <= "2021-03-14"] <- 3
pacientes$ola[pacientes$Fecha.de.Ingreso >= "2021-03-15" & pacientes$Fecha.de.Ingreso <= "2021-06-19"] <- 4
pacientes$ola[pacientes$Fecha.de.Ingreso >= "2021-06-20" & pacientes$Fecha.de.Ingreso <= "2021-10-13"] <- 5
pacientes$ola[pacientes$Fecha.de.Ingreso >= "2021-10-14" & pacientes$Fecha.de.Ingreso <= "2021-12-31"] <- 6
pacientes$ola <- as.factor(pacientes$ola)

##############################################################
#                                                            #
# FINAL DATASET                                              #
#                                                            #
##############################################################
library(zoo)

# Creating new (and final) dataset to plot time series
# "ingresos" in Spanish is "admissions" in English
ingresos <- data.frame(table(pacientes$Fecha.de.Ingreso) ) 
ingresos$Var1 <- as.Date(ingresos$Var1, "%Y-%m-%d")

# The library "zoo" is compulsory in order to use the rollmean() function
# to create a MOVING AVERAGE:
rolling_average <- rollmean(ingresos$Freq, 7) # 7-day moving average

rolling_average <- append(c(0,0,0,0,0,0), rolling_average)
ingresos <- cbind(ingresos, moving_average=rolling_average)
cum_ingresos <- cumsum(ingresos$Freq)
ingresos <- cbind(ingresos, acumulados=cum_ingresos)

write.csv(ingresos, "ingresos.csv")

##############################

# FINAL NOTE: if reserachers want to use the "mortality" dataset, it have to be rendered:
mortality <- data.frame(table(pacientes$Fecha.de.Ingreso[pacientes$Tipo.Alta == "4"] ) ) 
mortality$Var1 <- as.Date(mortalidad$Var1, "%Y-%m-%d")

# END OF FILE