## Cargamos librerías
library(dplyr)
library(stringr)
library(ggplot2)
##Leemos la data
data <-  read.csv("data_unificada.csv")


## Miramos lo que tenemos
str(data)
head(data)


## Creamos una columna aparte 
data <- data %>%
  mutate(CATEGORIA = ifelse(str_detect(CLIENTE, "Despacho a cliente"), "Despacho",
                            ifelse(str_detect(CLIENTE, "Faltante"), "Faltante",
                                   ifelse(str_detect(CLIENTE, "DEVOLUCION"), "Devolucion", "Despacho"))))

## Limpiamos la columna de clientes
data$CLIENTE <- sub("([|/].*)$", "", data$CLIENTE)

## Empezamos con la hipotesis 1
#El flujo de caja puede estar bajando debido a una ineficiencia en el sistema de distribucion
#Supuesto: Partiendo que la empresa tiene una politica de que sus pilotos deben realizar 
# minimo 22 entregas al mes


## entregas por piloto por mes
entregas_piloto <- data %>%
  group_by(PILOTO, MES,Year) %>% 
  summarise(cantidad_entregas = n())  %>% 
  

## promedio de entregas por piloto al año

promedio_entregas <- entregas_piloto %>%
  group_by(PILOTO) %>%
  summarise(promedio_entregas = mean(cantidad_entregas))

#Pilotos que han cumplido la politica

cantidad_pilotos <- entregas_piloto %>%
  group_by(PILOTO) %>% 
  filter(cantidad_entregas >= 23) %>% 
  summarise(Meses_cumplidos = n())  %>% 
  mutate(Porcentaje = Meses_cumplidos / 11)

## Entregas por mes
entregas_mes <- data %>%
  group_by(MES) %>% 
  summarise(cantidad_entregas = n())


## HIPOTESIS 2
# La cantidad de creditos dados puede estar afectando al flujo


porcentaje_credito <- sum(!is.na(data['CREDITO'])) / nrow(data) * 100

library(ggplot2)

# Gráfico de líneas para mostrar cómo varían los montos de las transacciones con el tiempo
ggplot(data, aes(x = MES, y = Q, color = CREDITO)) +
  geom_line() +
  labs(x = "Fecha", y = "Monto de la Transacción", color = "Crédito") +
  theme_minimal()


##Hipotesis 3
## Cantidad de devoluciones


devoluciones <- data %>% 
  group_by(MES) %>% 
  filter(CATEGORIA == "Devolucion") %>% 
  summarise(n_devoluciones = n())


