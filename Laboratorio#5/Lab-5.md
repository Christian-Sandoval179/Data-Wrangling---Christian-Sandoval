Lab#5
================

## R Markdown

Christian Sandoval Data Wrangling 2023

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 4.1.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.1.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 4.1.3

``` r
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.1.3

``` r
library(nycflights13)
```

    ## Warning: package 'nycflights13' was built under R version 4.1.3

## Parte 1

``` r
#  Calculamos los días en un Synodic Month
synodic_month_days <- 29 + (12/24) + (44/60/24) + (3/60/60/24) 

 # El número de Synodic Months en un Saros
saros_months <- 223
ultimo_eclipse <- as.POSIXct("2017-08-21 18:26:40", format = "%Y-%m-%d %H:%M:%S")


# Calculamos el número total de días
total_de_dias <- synodic_month_days * saros_months

# Calculamos la fecha del próximo eclipse solar

fecha_eclipse <- ultimo_eclipse + total_de_dias * 24 * 60 * 60 

# Ponemos la fecha en el mismo formato del ultimo eclipse
format(fecha_eclipse, format = "%d de %B de %Y a las %H:%M:%S")
```

    ## [1] "02 de September de 2035 a las 02:09:49"

## Parte 2

``` r
data <- read_xlsx("data.xlsx")

colnames(data) <- c("fecha_creacion", "hora_creacion", "callerID", "Cod", "email", "SMS", "call", "fecha_final", "hora_final")


#Convierto todas las fechas al mismo formato


data <- data %>% 
  mutate(
    fecha_creacion_origen = fecha_creacion,
    fecha_creacion = dmy(fecha_creacion),
    fecha_creacion_origen = as.Date(as.numeric(fecha_creacion_origen), origin = "1899-12-30"),
    fecha_final_origen = fecha_final,
    fecha_final = dmy(fecha_final),
    fecha_final_origen = as.Date(as.numeric(fecha_final_origen), origin = "1899-12-30")
  )
```

    ## Warning: There were 4 warnings in `mutate()`.
    ## The first warning was:
    ## i In argument: `fecha_creacion = dmy(fecha_creacion)`.
    ## Caused by warning:
    ## !  104237 failed to parse.
    ## i Run `dplyr::last_dplyr_warnings()` to see the 3 remaining warnings.

``` r
data <- data %>% 
  mutate(
    fecha_creacion_f = coalesce(fecha_creacion_origen, fecha_creacion),
    fecha_final_f = coalesce(fecha_final_origen, fecha_final)
  )


data <- data %>% 
  select(-fecha_creacion, -fecha_creacion_origen, -fecha_final_origen, -fecha_creacion_f)



## vemos los meses en los que existe mayor cantidad de llamadas por codigo
data <- data %>%
  mutate(mes = month(fecha_final_f))


resultado <- data %>%
  group_by(Cod, mes) %>%
  summarise(cantidad_llamadas = n())
```

    ## `summarise()` has grouped output by 'Cod'. You can override using the `.groups`
    ## argument.

``` r
resultado <- resultado %>%
  group_by(Cod) %>%
  filter(cantidad_llamadas == max(cantidad_llamadas))

print(resultado)
```

    ## # A tibble: 7 x 3
    ## # Groups:   Cod [7]
    ##   Cod                            mes cantidad_llamadas
    ##   <chr>                        <dbl>             <int>
    ## 1 0                                7              1463
    ## 2 Actualización de Información     5              1691
    ## 3 Cancelaciones                    3              4092
    ## 4 Cobros                           1               688
    ## 5 Consultas                       10             10790
    ## 6 Empresarial                     10              3136
    ## 7 Otros/Varios                     1              1129

``` r
## Vemos el dia mas ocupado de la semana


data <- data %>%
  mutate(dia_semana = weekdays(fecha_final_f))


resultado2 <- data %>%
  group_by(dia_semana) %>%
  summarise(cantidad_llamadas = n())


dia_mas_ocupado <- resultado2 %>%
  filter(cantidad_llamadas == max(cantidad_llamadas)) %>%
  select(dia_semana)

print(resultado2)
```

    ## # A tibble: 7 x 2
    ##   dia_semana cantidad_llamadas
    ##   <chr>                  <int>
    ## 1 Friday                 37409
    ## 2 Monday                 37501
    ## 3 Saturday               37614
    ## 4 Sunday                 38254
    ## 5 Thursday               37726
    ## 6 Tuesday                37710
    ## 7 Wednesday              37511

``` r
## Mes mas ocupado


resultado3 =  data %>% 
  group_by(mes) %>% 
   summarise(cantidad_comunicaciones = n())

mes_mas_ocupado <- resultado3 %>%
  filter(cantidad_comunicaciones == max(cantidad_comunicaciones)) %>%
  select(mes)

  paste0("El mes mas ocupado es el mes: ", mes_mas_ocupado)
```

    ## [1] "El mes mas ocupado es el mes: 3"

``` r
## Existe una gran concentracion de llamadas en el mes de Octubre
  
  
## Duracion de la llamada promedio
  data <- data %>%
    filter(call == 1) %>% 
  mutate(duracion_llamada_minutos = as.numeric(difftime(hora_final, hora_creacion, units = "mins")))

  promedio_duracion <- mean(data$duracion_llamada_minutos)
    
  paste0("La duracion promedio de llamada es de: ", promedio_duracion)
```

    ## [1] "La duracion promedio de llamada es de: 14.557903930131"

``` r
## Tabla de frecuencias
  
  tabla_f <- table(data$duracion_llamada_minutos)
  
  tabla_df <- as.data.frame(tabla_f)
  colnames(tabla_df) <- c("Duracion_Llamada", "Frecuencia")
  
  print(tabla_df)
```

    ##    Duracion_Llamada Frecuencia
    ## 1                 0        221
    ## 2                 1        211
    ## 3                 2        173
    ## 4                 3        195
    ## 5                 4        193
    ## 6                 5        184
    ## 7                 6        194
    ## 8                 7        197
    ## 9                 8        212
    ## 10                9        166
    ## 11               10        190
    ## 12               11        197
    ## 13               12        169
    ## 14               13        163
    ## 15               14        203
    ## 16               15        188
    ## 17               16        181
    ## 18               17        178
    ## 19               18        186
    ## 20               19        190
    ## 21               20        179
    ## 22               21        205
    ## 23               22        175
    ## 24               23        192
    ## 25               24        186
    ## 26               25        174
    ## 27               26        157
    ## 28               27        173
    ## 29               28        158
    ## 30               29        171
    ## 31               30        164

``` r
  ## Como comentario adicional podemos ver que la mayor frecuencia se concentra en su media que son los 14 minutos promedio por llamada
  
  
##
```

## Parte 3

``` r
obtener_signo <- function(fecha_nacimiento) {
  
  dia <- day(fecha_nacimiento)
  mes <- month(fecha_nacimiento)
  

  if ((mes == 3 && dia >= 21) || (mes == 4 && dia <= 19)) {
    signo <- "Aries"
  } else if ((mes == 4 && dia >= 20) || (mes == 5 && dia <= 20)) {
    signo <- "Tauro"
  } else if ((mes == 5 && dia >= 21) || (mes == 6 && dia <= 20)) {
    signo <- "Géminis"
  } else if ((mes == 6 && dia >= 21) || (mes == 7 && dia <= 22)) {
    signo <- "Cáncer"
  } else if ((mes == 7 && dia >= 23) || (mes == 8 && dia <= 22)) {
    signo <- "Leo"
  } else if ((mes == 8 && dia >= 23) || (mes == 9 && dia <= 22)) {
    signo <- "Virgo"
  } else if ((mes == 9 && dia >= 23) || (mes == 10 && dia <= 22)) {
    signo <- "Libra"
  } else if ((mes == 10 && dia >= 23) || (mes == 11 && dia <= 21)) {
    signo <- "Escorpio"
  } else if ((mes == 11 && dia >= 22) || (mes == 12 && dia <= 21)) {
    signo <- "Sagitario"
  } else if ((mes == 12 && dia >= 22) || (mes == 1 && dia <= 19)) {
    signo <- "Capricornio"
  } else {
    signo <- "Acuario"
  }
  
  return(signo)
}

## Ingrese su fecha en el siguiente formata Y% Month% Day%

fecha_nacimiento <- as.Date("2002-04-04")
signo_zodiacal <- obtener_signo(fecha_nacimiento)

paste0(" Tu signo zodiacal es: " , signo_zodiacal , " dada tu fecha de nacimiento: " , fecha_nacimiento)
```

    ## [1] " Tu signo zodiacal es: Aries dada tu fecha de nacimiento: 2002-04-04"

## Parte 4

``` r
flights <- flights

flights <- flights %>% 
  mutate(dep_time_enhora = sprintf("%02d:%02d", dep_time %/% 100, dep_time %% 100)) %>% 
  mutate(arr_time_enhora = sprintf("%02d:%02d", arr_time %/% 100, arr_time %% 100)) %>% 
  mutate(sched_dep_time_enhora = sprintf("%02d:%02d", sched_dep_time %/% 100, sched_dep_time %% 100)) %>% 
  mutate(sched_arr_time_enhora = sprintf("%02d:%02d", sched_arr_time %/% 100, sched_arr_time %% 100))


# Encontrando el delay total seria:


flights <- flights %>% 
  mutate(delay_total = dep_delay + arr_delay)
```
