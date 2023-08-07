Lab# 1
================

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

``` r
## ----------------- PROBLEMA 1 ------------------------------------
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.1.3

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 4.1.3

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.3

    ## Warning: package 'ggplot2' was built under R version 4.1.3

    ## Warning: package 'tibble' was built under R version 4.1.3

    ## Warning: package 'tidyr' was built under R version 4.1.3

    ## Warning: package 'purrr' was built under R version 4.1.3

    ## Warning: package 'dplyr' was built under R version 4.1.3

    ## Warning: package 'stringr' was built under R version 4.1.3

    ## Warning: package 'forcats' was built under R version 4.1.3

    ## Warning: package 'lubridate' was built under R version 4.1.3

    ## -- Attaching core tidyverse packages ------------------------ tidyverse 2.0.0 --
    ## v dplyr     1.1.2     v purrr     1.0.1
    ## v forcats   1.0.0     v stringr   1.5.0
    ## v ggplot2   3.4.2     v tibble    3.2.1
    ## v lubridate 1.9.2     v tidyr     1.3.0
    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()
    ## i Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(tidytext)
```

    ## Warning: package 'tidytext' was built under R version 4.1.3

``` r
library(dbplyr)
```

    ## 
    ## Attaching package: 'dbplyr'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     ident, sql

``` r
data_unificada <- read_csv("data_unificada.csv")
```

    ## Rows: 2180 Columns: 9
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (4): CLIENTE, PILOTO, UNIDAD, Fecha
    ## dbl (5): COD_VIAJE, UBICACION, CANTIDAD, Q, CREDITO
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(data_unificada)
```

    ## # A tibble: 6 x 9
    ##   COD_VIAJE CLIENTE         UBICACION CANTIDAD PILOTO     Q CREDITO UNIDAD Fecha
    ##       <dbl> <chr>               <dbl>    <dbl> <chr>  <dbl>   <dbl> <chr>  <chr>
    ## 1  10000001 EL PINCHE OBEL~     76002     1200 Ferna~ 300        30 Camio~ 01-2~
    ## 2  10000002 TAQUERIA EL CH~     76002     1433 Hecto~ 358.       90 Camio~ 01-2~
    ## 3  10000003 TIENDA LA BEND~     76002     1857 Pedro~ 464.       60 Camio~ 01-2~
    ## 4  10000004 TAQUERIA EL CH~     76002      339 Angel~  84.8      30 Panel  01-2~
    ## 5  10000005 CHICHARRONERIA~     76001     1644 Juan ~ 411        30 Camio~ 01-2~
    ## 6  10000006 UBIQUO LABS ||~     76001     1827 Luis ~ 457.       30 Camio~ 01-2~

``` r
paste("El numero de filas del archivo es :" ,nrow(data_unificada) )
```

    ## [1] "El numero de filas del archivo es : 2180"

``` r
paste("El numero de columnas del archivo es :" ,ncol(data_unificada) )
```

    ## [1] "El numero de columnas del archivo es : 9"

``` r
## ----------------- PROBLEMA 2 ------------------------------------

lista_vectores <- list(vec1 = c(1,2,3,3,4,4,5,8,9,7) , 
                       vec2 = c(3,4,3,3,11,16,3,20) , 
                       vec3 = c(22,35,35,21,21,35,99))

calculo_moda <- function(vector) {
  frecuencias <- table(vector)
  moda <- as.numeric(names(frecuencias[frecuencias == max(frecuencias)]))
  return(moda)
}

moda_vectores <- lapply(lista_vectores, calculo_moda)
moda_vectores
```

    ## $vec1
    ## [1] 3 4
    ## 
    ## $vec2
    ## [1] 3
    ## 
    ## $vec3
    ## [1] 35

``` r
## ----------------- PROBLEMA 3 ------------------------------------

datos_parqueo2019 <- read_delim("parqueo2019.txt" , "|")
```

    ## New names:
    ## * `` -> `...11`

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 2435294 Columns: 11
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: "|"
    ## chr (8): MES, NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO, MODELO_VEHICULO, LINEA_...
    ## dbl (2): ANIO_ALZA, CANTIDAD
    ## lgl (1): ...11
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(datos_parqueo2019, 10)
```

    ## # A tibble: 10 x 11
    ##    ANIO_ALZA MES   NOMBRE_DEPARTAMENTO NOMBRE_MUNICIPIO MODELO_VEHICULO
    ##        <dbl> <chr> <chr>               <chr>            <chr>          
    ##  1      2007 05    HUEHUETENANGO       "HUEHUETENANGO"  2007           
    ##  2      2007 05    EL PROGRESO         "EL JICARO"      2007           
    ##  3      2007 05    SAN MARCOS          "OCOS"           2007           
    ##  4      2007 05    ESCUINTLA           "SAN JOS\xc9"    2006           
    ##  5      2007 05    JUTIAPA             "MOYUTA"         2007           
    ##  6      2007 05    GUATEMALA           "FRAIJANES"      1997           
    ##  7      2007 05    QUETZALTENANGO      "QUETZALTENANGO" 2007           
    ##  8      2007 05    SUCHITEPEQUEZ       "CHICACAO"       2007           
    ##  9      2007 05    ESCUINTLA           "ESCUINTLA"      2007           
    ## 10      2007 05    GUATEMALA           "MIXCO"          2007           
    ## # i 6 more variables: LINEA_VEHICULO <chr>, TIPO_VEHICULO <chr>,
    ## #   USO_VEHICULO <chr>, MARCA_VEHICULO <chr>, CANTIDAD <dbl>, ...11 <lgl>
