Lab# 1
================

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

``` r
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

## Including Plots

You can also embed plots, for example:

![](lab-1_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
