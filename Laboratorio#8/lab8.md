Lab8
================
Christian Sandoval
10/29/2023

## Laboratorio#8 Feature Engineering

Data Wrangling 2023 Christian Sandoval - 20210393

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
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.1.3

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 4.1.3

    ## Loading required package: lattice

## Leyendo la data

``` r
#Data que tiene missing data
data <- read_csv("titanic_MD.csv") 
```

    ## Rows: 183 Columns: 12
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (5): Name, Sex, Ticket, Cabin, Embarked
    ## dbl (7): PassengerId, Survived, Pclass, Age, SibSp, Parch, Fare
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Data completa para comparar

data_completa <- read_csv("titanic.csv")
```

    ## Rows: 183 Columns: 12
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (5): Name, Sex, Ticket, Cabin, Embarked
    ## dbl (7): PassengerId, Survived, Pclass, Age, SibSp, Parch, Fare
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Reporte de Missing Data y Columnas completas

``` r
 missing_data <- colSums(is.na(data))
columnas_con_faltantes <- names(missing_data[missing_data > 0])
columnas_completas <- names(missing_data[missing_data == 0])


#Resumen de todas las columnas
print(missing_data)
```

    ## PassengerId    Survived      Pclass        Name         Sex         Age 
    ##           0           0           0           0           0          25 
    ##       SibSp       Parch      Ticket        Fare       Cabin    Embarked 
    ##           3          12           0           8           0          12

``` r
#Columnas que tienen NA
print(columnas_con_faltantes)
```

    ## [1] "Age"      "SibSp"    "Parch"    "Fare"     "Embarked"

``` r
#Columnas que estan completas
# Aqui tenemos que tomar en cuenta que la columna de 'Sex' esta completa pero hay campos que tienen un "?"
print(columnas_completas)
```

    ## [1] "PassengerId" "Survived"    "Pclass"      "Name"        "Sex"        
    ## [6] "Ticket"      "Cabin"

``` r
# Vemos los complete cases de toda la data

complete.cases(data) %>% table()
```

    ## .
    ## FALSE  TRUE 
    ##    47   136

``` r
# Podemos ver que de los 183 registros, solo tenemos 136 que estan completos, esto incluye los registros que tienen el "?" en la columna
# de sex
```

## Especificamos como vamos a trabajar con cada columna de missing data

Age: Imputacion por mediana, porque de esta forma, no tendriamos
problemas con valores atipicos que puedan alterar la distribucion de los
datos como podria suceder con la media.

SibSp: Imputacion por mediana, porque de esta forma, no tendriamos
problemas con valores atipicos que puedan alterar la distribucion de los
datos como podria suceder con la media.

Parch: Imputacion por media, al ser padres/hijos sabemos que la
probabilidad de datos atipicos es muy baja por lo que una medida como la
media nos podria servir

Fare: Al saber que tenemos diferentes clases economicas para ‘Fare’ se
podria hacer una imputacion por la media segun cada ‘Pclass’ que
tengamos.

Embarked: Al ser una columna con variables categoricas, podriamos
utilizar imputacion por la moda de esta forma llenariamos los missing
values con la variable que mas sale en esta columna que en este caso
seria por el puerto que mas embarcaron.

## TRABAJAMOS LAS COLUMNAS CON MISSSING VALUES

``` r
# Econtrar moda para columna categorica
find.mode_categoria <- function() {

  embarked <- data %>% 
    group_by(Embarked) %>% 
    summarise(count = n()) %>%
     arrange(desc(count))
  
  moda <- embarked$Embarked[1]
  
  return(moda)
    
}

#Encontrar moda para columna numerica
find.mode <- function(columna) {

  moda <- names(sort(table(columna), decreasing = TRUE))[1]
  
  return(moda)
  
    
}
```

## Con imputacion general

``` r
#Para 'age'
data$Age[is.na(data$Age)] <- mean(data$Age , na.rm = TRUE)
data$Age[is.na(data$Age)] <- median(data$Age , na.rm = TRUE)
data$Age[is.na(data$Age)] <- find.mode(data$Age)

#Para 'SibSp'

data$SibSp[is.na(data$SibSp)] <- mean(data$SibSp, na.rm = TRUE)
data$SibSp[is.na(data$SibSp)] <- median(data$SibSp, na.rm = TRUE)
data$SibSp[is.na(data$SibSp)] <- find.mode(data$SibSp)

# Para 'Parch'

data$Parch[is.na(data$Parch)] <- mean(data$Parch, na.rm = TRUE)
data$Parch[is.na(data$Parch)] <- median(data$Parch, na.rm = TRUE)
data$Parch[is.na(data$Parch)] <- find.mode(data$Parch)

# Para 'Fare'
data$Fare[is.na(data$Fare)] <- mean(data$Fare , na.rm = TRUE)
data$Fare[is.na(data$Fare)] <- median(data$Fare , na.rm = TRUE)
data$Fare[is.na(data$Fare)] <- find.mode(data$Fare)

# Para 'embarked'
data$Embarked[is.na(data$Embarked)] <- find.mode_categoria()
```

## Con Linear Regression

``` r
#Para Age

known_age <- data[complete.cases(data$Age), ]
missing_age <- data[!complete.cases(data$Age), ]


modelo_edad <- lm(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = known_age)
predicciones_edad <- predict(modelo_edad, newdata = missing_age)
```

    ## Warning in predict.lm(modelo_edad, newdata = missing_age): prediction from a
    ## rank-deficient fit may be misleading

``` r
data$Age[!complete.cases(data$Age)] <- predicciones_edad


#Para SibSp

known_sib <- subset(data, !is.na(SibSp))  
missing_sib <- subset(data, is.na(SibSp))  




modelo_sib <-lm(SibSp ~ Pclass + Sex + Age + Parch + Fare + Embarked, data = known_sib)
predicciones_sib <- predict(modelo_sib, newdata = missing_sib)
```

    ## Warning in predict.lm(modelo_sib, newdata = missing_sib): prediction from a
    ## rank-deficient fit may be misleading

``` r
data$SibSp[is.na(data$SibSp)] <- predicciones_sib

#Para Parch

known_parch <- data[complete.cases(data), ]
missing_parch <- data[!complete.cases(data), ]


modelo_parch <- lm(Parch ~ Age + Fare, data = known_parch)
predicciones_parch <- predict(modelo_parch, newdata = missing_parch)
```

    ## Warning in predict.lm(modelo_parch, newdata = missing_parch): prediction from a
    ## rank-deficient fit may be misleading

``` r
data$Parch[!complete.cases(data$Parch)] <- predicciones_parch

#Para Fare

data_entrenamiento <- data %>% filter(!is.na(Fare))
modelo_fare <- lm(Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked, data = data_entrenamiento)
  

data_prediccion <- data %>% filter(is.na(Fare))
predicciones_fare <- predict(modelo_fare, newdata = data_prediccion)
```

    ## Warning in predict.lm(modelo_fare, newdata = data_prediccion): prediction from
    ## a rank-deficient fit may be misleading

``` r
data$Fare[is.na(data$Fare)] <- predicciones_fare
```

## Outliers

``` r
#Para Age
x <-  2

lower_age <- mean(data$Age, na.rm = T) -  (sd(data$Age, na.rm = T) * x)
```

    ## Warning in mean.default(data$Age, na.rm = T): argument is not numeric or
    ## logical: returning NA

``` r
upper_age <- mean(data$Age, na.rm = T) +  (sd(data$Age, na.rm = T) * x)
```

    ## Warning in mean.default(data$Age, na.rm = T): argument is not numeric or
    ## logical: returning NA

``` r
data[(data$Age >= lower_age) & (data$Age <= upper_age) & (!is.na(data$Age)), ]
```

    ## # A tibble: 183 x 12
    ##    PassengerId Survived Pclass Name  Sex   Age   SibSp Parch Ticket Fare  Cabin
    ##          <dbl>    <dbl>  <dbl> <chr> <chr> <chr> <chr> <chr> <chr>  <chr> <chr>
    ##  1          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  2          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  3          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  4          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  5          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  6          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  7          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  8          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  9          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ## 10          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ## # i 173 more rows
    ## # i 1 more variable: Embarked <chr>

``` r
#Para SibSp

x2 <- 3

lower_sib <- mean(data$SibSp, na.rm = T) -  (sd(data$SibSp, na.rm = T) * x2)
```

    ## Warning in mean.default(data$SibSp, na.rm = T): argument is not numeric or
    ## logical: returning NA

``` r
upper_sib <- mean(data$SibSp, na.rm = T) +  (sd(data$SibSp, na.rm = T) * x2)
```

    ## Warning in mean.default(data$SibSp, na.rm = T): argument is not numeric or
    ## logical: returning NA

``` r
data[(data$SibSp >= lower_sib) & (data$SibSp <= upper_sib) & (!is.na(data$SibSp)), ]
```

    ## # A tibble: 183 x 12
    ##    PassengerId Survived Pclass Name  Sex   Age   SibSp Parch Ticket Fare  Cabin
    ##          <dbl>    <dbl>  <dbl> <chr> <chr> <chr> <chr> <chr> <chr>  <chr> <chr>
    ##  1          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  2          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  3          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  4          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  5          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  6          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  7          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  8          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  9          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ## 10          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ## # i 173 more rows
    ## # i 1 more variable: Embarked <chr>

``` r
#Para Parch 

x3 <- 2
lower_parch<- mean(data$Parch, na.rm = T) -  (sd(data$Parch, na.rm = T) * x3)
```

    ## Warning in mean.default(data$Parch, na.rm = T): argument is not numeric or
    ## logical: returning NA

``` r
upper_parch <- mean(data$Parch, na.rm = T) +  (sd(data$Parch, na.rm = T) * x3)
```

    ## Warning in mean.default(data$Parch, na.rm = T): argument is not numeric or
    ## logical: returning NA

``` r
data[(data$Parch >= lower_parch) & (data$Parch <= upper_parch) & (!is.na(data$Parch)), ]
```

    ## # A tibble: 183 x 12
    ##    PassengerId Survived Pclass Name  Sex   Age   SibSp Parch Ticket Fare  Cabin
    ##          <dbl>    <dbl>  <dbl> <chr> <chr> <chr> <chr> <chr> <chr>  <chr> <chr>
    ##  1          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  2          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  3          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  4          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  5          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  6          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  7          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  8          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  9          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ## 10          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ## # i 173 more rows
    ## # i 1 more variable: Embarked <chr>

``` r
#Para Fare

x4 <- 2
lower_fare<- mean(data$Fare, na.rm = T) -  (sd(data$Fare, na.rm = T) * x4)
```

    ## Warning in mean.default(data$Fare, na.rm = T): argument is not numeric or
    ## logical: returning NA

``` r
upper_fare <- mean(data$Fare, na.rm = T) +  (sd(data$Fare, na.rm = T) * x4)
```

    ## Warning in mean.default(data$Fare, na.rm = T): argument is not numeric or
    ## logical: returning NA

``` r
data[(data$Fare >= lower_fare) & (data$Fare <= upper_fare) & (!is.na(data$Fare)), ]
```

    ## # A tibble: 183 x 12
    ##    PassengerId Survived Pclass Name  Sex   Age   SibSp Parch Ticket Fare  Cabin
    ##          <dbl>    <dbl>  <dbl> <chr> <chr> <chr> <chr> <chr> <chr>  <chr> <chr>
    ##  1          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  2          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  3          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  4          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  5          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  6          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  7          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  8          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ##  9          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ## 10          NA       NA     NA <NA>  <NA>  <NA>  <NA>  <NA>  <NA>   <NA>  <NA> 
    ## # i 173 more rows
    ## # i 1 more variable: Embarked <chr>

## Comparacion de los metodos utilizados

``` r
summary(data_completa$Age)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.92   24.00   36.00   35.67   47.50   80.00

``` r
summary(known_age$Age)
```

    ##    Length     Class      Mode 
    ##       183 character character

``` r
summary(data_completa$SibSp)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.0000  0.0000  0.4645  1.0000  3.0000

``` r
summary(known_sib$SibSp)
```

    ##    Length     Class      Mode 
    ##       183 character character

``` r
summary(data_completa$Parch)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.0000  0.0000  0.4754  1.0000  4.0000

``` r
summary(known_parch)
```

    ##   PassengerId       Survived          Pclass          Name          
    ##  Min.   :  2.0   Min.   :0.0000   Min.   :1.000   Length:183        
    ##  1st Qu.:263.5   1st Qu.:0.0000   1st Qu.:1.000   Class :character  
    ##  Median :457.0   Median :1.0000   Median :1.000   Mode  :character  
    ##  Mean   :455.4   Mean   :0.6721   Mean   :1.191                     
    ##  3rd Qu.:676.0   3rd Qu.:1.0000   3rd Qu.:1.000                     
    ##  Max.   :890.0   Max.   :1.0000   Max.   :3.000                     
    ##      Sex                Age               SibSp              Parch          
    ##  Length:183         Length:183         Length:183         Length:183        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##     Ticket              Fare              Cabin             Embarked        
    ##  Length:183         Length:183         Length:183         Length:183        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ## 

``` r
summary(data_completa$Fare)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.00   29.70   57.00   78.68   90.00  512.33

``` r
summary(data_entrenamiento$Fare)
```

    ##    Length     Class      Mode 
    ##       183 character character

``` r
data_embarked <- data_completa %>% 
   group_by(Embarked) %>% 
    summarise(count = n()) %>%
     arrange(desc(count))


data_conmoda <- data %>% 
   group_by(Embarked) %>% 
    summarise(count = n()) %>%
     arrange(desc(count))
```

Para la columna de Age , la mejor forma fue por medio de la imputacion
por mediana, manteniendo una distribucion constante en la variable sin
presentar algun sesgo. Evitando datos atipicos

Para las columnas de SibSp y Parch , el mejor approach fue la regresion
lineal, al tener mas informacion con la cual establecer el numero de
SibSp/Parch sin solo sacar una media o mediana o moda de esta columna

Para Fare, igual la regresion lineal fue el mejor acercamiento, ya que
podiamos establecer cuales variables tenian mas peso para predecir ese
valor.

Para Embarked, sin ninguna duda, el mejor metodo fue la imputacion por
moda, al ser una variable categorica , podiamos usar este metodo, que
dio resultados muy parecidos comparado con el actual

## Conclusiones

Media, mediana, moda: Estas son estrategias comunes para imputar valores
faltantes. NUestra eleccion entre ellas depende de la naturaleza de los
datos y el impacto en el análisis. La media es sensible a valores
extremos, la mediana es más robusta y la moda es útil para datos
categóricos o discretos.

Media, mediana, moda: Estas son estrategias comunes para imputar valores
faltantes. La elección entre ellas depende de la naturaleza de los datos
y el impacto en el análisis. La media es sensible a valores extremos, la
mediana es más robusta y la moda es útil para datos categóricos o
discretos.

La elección de la estrategia de imputación debe basarse en el contexto
del problema y la naturaleza de los datos. Es crucial validar cualquier
enfoque de imputación utilizado.
