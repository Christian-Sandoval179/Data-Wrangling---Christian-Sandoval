install.packages("tidyverse")
library(readxl)
library(readr)
library(tidyverse)
library(tidytext)
library(dbplyr)

fechas <- c("01-2018.xlsx","02-2018.xlsx","03-2018.xlsx",
              "04-2018.xlsx","05-2018.xlsx","06-2018.xlsx",
              "07-2018.xlsx","08-2018.xlsx","09-2018.xlsx",
              "10-2018.xlsx","11-2018.xlsx")

procesar_datos <-  function(archivos){
  archivos1 <- read_excel(archivos)
  archivos1 <- select(archivos1, COD_VIAJE, CLIENTE, UBICACION, CANTIDAD, PILOTO, Q, CREDITO, UNIDAD)
  
  mes<- substr(archivos, 1, 2)
  year <- substr(archivos, 4, 7)
  archivos1$Fecha <- paste0(mes, "-", year)
  return(archivos1)
}

data <- lapply(fechas, procesar_datos)

data_unificada <- bind_rows(data)

write_csv(data_unificada, path = "data_unificada.csv")

