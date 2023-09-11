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


cantidad_pilotos$ID <- seq(1, nrow(cantidad_pilotos))
  promedio_meses <- mean(cantidad_pilotos$Meses_cumplidos)
  
  # Grafico de meses cumplidos por piloto
  ggplot(cantidad_pilotos, aes(x = ID, y = Meses_cumplidos)) +
    geom_bar(stat = "identity", fill = "steelblue1") + 
    geom_text(aes(label = Meses_cumplidos), vjust = -0.5, size = 3) +
    labs(x = " ID Piloto", y = "Meses Cumplidos", title = "Cumplimiento de Politica de Entregas")+
    scale_x_continuous(breaks = 1:9)
  

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
## Cantidad de devoluciones han aumentado en ciertos meses


devoluciones <- data %>% 
  group_by(MES) %>% 
  filter(CATEGORIA == "Devolucion") %>% 
  summarise(n_devoluciones = n())

ggplot(devoluciones, aes(x = MES, y = n_devoluciones)) +
  geom_bar(stat = "sum", fill = "red", alpha = 0.6) +
  labs(x = "Mes", y = "Numero de devoluciones", title = "Cantidad de Devoluciones por Mes")


#Las devoluciones estan relacionadas con la cantidad de unidades que se han entregado


devolucion_totales <- devolucion %>% 
  group_by(MES) %>% 
  summarise(devolucion_total = sum(CANTIDAD))


## Grafica de devoluciones (unidades) por mes   


ggplot(devolucion_totales, aes(x = MES, y = devolucion_total)) +
  geom_bar(stat = "sum", position = "stack", fill = "lightgreen") + 
  geom_text(aes(label = devolucion_total), position = position_stack(vjust = 0.5), color = "black", size = 3) +
  labs(x = "Mes", y = "Unidades Devueltas", title = "Cantidad de Unidades Devueltas por Mes") +
  scale_x_continuous(breaks = 1:9)+
  theme_minimal()



## Hipotesis 4, no todos los medios de transportes son rentables
D
ingresos_por_unidad <- data %>%
  group_by(UNIDAD) %>% 
  filter(CATEGORIA == 'Despacho') %>% 
  summarise(ingresos_totales = sum(Q)) %>%
  mutate(porcentaje_ingresos = ingresos_totales / sum(ingresos_totales) * 100)

## Grafica de ingresos por unidad

ingresos_por_unidad$porcentaje_ingresos <- round(ingresos_por_unidad$porcentaje_ingresos)

ggplot(ingresos_por_unidad, aes(x = factor(1), y = porcentaje_ingresos, fill = UNIDAD, label = paste(porcentaje_ingresos, "%"))) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), color = "white", size = 3)+
  labs(x = "Barra", y = "Porcentaje de Ingresos", title = "Distribución de Ingresos por Unidad") +
  scale_fill_manual(values = c("#FFA500", "#1E90FF","#339966")) +
  theme_minimal()


entregas_por_camion <- data %>% 
  group_by(UNIDAD) %>% 
  summarise(entregas_totales = n())


## Entender la relacion 80/20 de la empresa
