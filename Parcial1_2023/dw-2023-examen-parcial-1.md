dw-2023-parcial-1
================
Tepi
9/11/2023

# Examen parcial

Indicaciones generales:

-   Usted tiene el período de la clase para resolver el examen parcial.

-   La entrega del parcial, al igual que las tareas, es por medio de su
    cuenta de github, pegando el link en el portal de MiU.

-   Pueden hacer uso del material del curso e internet (stackoverflow,
    etc.). Sin embargo, si encontramos algún indicio de copia, se
    anulará el exámen para los estudiantes involucrados. Por lo tanto,
    aconsejamos no compartir las agregaciones que generen.

## Sección 0: Preguntas de temas vistos en clase (20pts)

-   Responda las siguientes preguntas de temas que fueron tocados en
    clase.

1.  ¿Qué es una ufunc y por qué debemos de utilizarlas cuando
    programamos trabajando datos?

``` r
    ## A diferencia de una función normal una , ufunc es una funcion universal que nos permite que la apliquemos a diferentes arreglos ya sea 1 o mas.
```

2.  Es una técnica en programación numérica que amplía los objetos que
    son de menor dimensión para que sean compatibles con los de mayor
    dimensión. Describa cuál es la técnica y de un breve ejemplo en R.

``` r
    #La tecnica es la que se llama broadcasting. Un ejemplo de esta puede ser sumando un vector
   # especifico a las filas de una matriz.
```

3.  ¿Qué es el axioma de elegibilidad y por qué es útil al momento de
    hacer análisis de datos?

4.  Cuál es la relación entre la granularidad y la agregación de datos?
    Mencione un breve ejemplo. Luego, exploque cuál es la granularidad o
    agregación requerida para poder generar un reporte como el
    siguiente:

``` r
       # LA granularidad en una tabla es por decir así es la unidad mínima de detalle de los datos por la que podemos agrupar. Tienen una relacion inversamente proporcional. Al tener un nivel mas bajo de granularidad las agrupaciones pueden tener mas informacion y al reves. Un ejemplo de granularidad puede ser la fecha en una tabla de ventas. En el ejemplo la granularidad seria el pais.
```

| Pais | Usuarios |
|------|----------|
| US   | 1,934    |
| UK   | 2,133    |
| DE   | 1,234    |
| FR   | 4,332    |
| ROW  | 943      |

## Sección I: Preguntas teóricas. (50pts)

-   Existen 10 preguntas directas en este Rmarkdown, de las cuales usted
    deberá responder 5. Las 5 a responder estarán determinadas por un
    muestreo aleatorio basado en su número de carné.

-   Ingrese su número de carné en `set.seed()` y corra el chunk de R
    para determinar cuáles preguntas debe responder.

``` r
#set.seed("20210393") 
v<- 1:10
preguntas <-sort(sample(v, size = 5, replace = FALSE ))

paste0("Mis preguntas a resolver son: ",paste0(preguntas,collapse = ", "))
```

    ## [1] "Mis preguntas a resolver son: 1, 3, 4, 6, 10"

``` r
### Mis preguntas son las 3, 7 , 8 , 8 , 10
```

### Listado de preguntas teóricas

3.  ¿Por qué en R utilizamos funciones de la familia apply
    (lapply,vapply) en lugar de utilizar ciclos?

``` r
## Usamos las funciones lapply porque es más eficiente al momento de aplicar funciones a objetos que queremos que usando ciclos y  porque también nos brinda una sentencia más estructurada 
```

7.  ¿Qué pasa si quiero agregar una nueva categoría a un factor que no
    se encuentra en los niveles existentes?

``` r
 ## Se genera un NA
```

8.  Si en un dataframe, a una variable de tipo `factor` le agrego un
    nuevo elemento que *no se encuentra en los niveles existentes*,
    ¿cuál sería el resultado esperado y por qué?
    -   El nuevo elemento
    -   `NA`

``` r
## Seria el nuevo elemento , ya que al no estar en los niveles existentes se crearia un nuevo nivel para este elemento
```

9.  En SQL, ¿para qué utilizamos el keyword `HAVING`?

``` r
      ## Usamos HAVING en sql al momento de tener agregaciones, ya que en estas no podemos usar el key word WHERE. Por lo tanto usamos HAVING para especificar las condiciones por las que queremos filtrar
```

10. Si quiero obtener como resultado las filas de la tabla A que no se
    encuentran en la tabla B, ¿cómo debería de completar la siguiente
    sentencia de SQL?

    -   SELECT \* FROM A LEFT JOIN B ON A.KEY = B.KEY WHERE B.KEY = NULL

Extra: ¿Cuántos posibles exámenes de 5 preguntas se pueden realizar
utilizando como banco las diez acá presentadas? (responder con código de
R.)

## Sección II Preguntas prácticas. (30pts)

-   Conteste las siguientes preguntas utilizando sus conocimientos de R.
    Adjunte el código que utilizó para llegar a sus conclusiones en un
    chunk del markdown.

A. De los clientes que están en más de un país,¿cuál cree que es el más
rentable y por qué?

B. Estrategia de negocio ha decidido que ya no operará en aquellos
territorios cuyas pérdidas sean “considerables”. Bajo su criterio,
¿cuáles son estos territorios y por qué ya no debemos operar ahí?

### I. Preguntas teóricas

``` r
  library(dplyr)
  library(readr)
```

    ## Warning: package 'readr' was built under R version 4.1.3

``` r
data <- read_rds("parcial_anonimo.rds")
```

## A

``` r
###resuelva acá

  rentabilidad_promedio <- data %>%
  group_by(Cliente) %>%
  summarise(RentabilidadPromedio = mean(Venta))

  #clientes que estan en mas de 1 pais
  clientes <- data %>%
  group_by(Cliente) %>%
  summarise(Cantidad_Paises = n_distinct(Pais)) %>%
  filter(Cantidad_Paises > 1)

  print(clientes)
```

    ## # A tibble: 7 x 2
    ##   Cliente  Cantidad_Paises
    ##   <chr>              <int>
    ## 1 044118d4               2
    ## 2 a17a7558               2
    ## 3 bf1e94e9               2
    ## 4 c53868a0               2
    ## 5 f2aab44e               2
    ## 6 f676043b               2
    ## 7 ff122c3f               2

``` r
  # nombre de estos clientes
  nombres_clientes <- clientes %>%
  pull(Cliente)
  
  final <- rentabilidad_promedio %>%
  filter(Cliente %in% nombres_clientes)
  
  print(final)
```

    ## # A tibble: 7 x 2
    ##   Cliente  RentabilidadPromedio
    ##   <chr>                   <dbl>
    ## 1 044118d4                14.1 
    ## 2 a17a7558                22.6 
    ## 3 bf1e94e9                 0   
    ## 4 c53868a0                19.2 
    ## 5 f2aab44e                 8.90
    ## 6 f676043b                12.7 
    ## 7 ff122c3f                22.0

``` r
  ## el cliente mas rentable es "a17a7558" ya que me presenta la mayor rentabilidad promedio , esto significa que es el cliente con el que podria tener mas ventas
```

## B

``` r
###resuelva acá
promedio_ventas_territorio <- data %>%
  group_by(Territorio) %>%
  summarise(Ventas = sum(Venta)) %>% 
  filter(Ventas < 1000)

# nombre de los territorios donde vamos a dejar de operar
  nombres_territorios <- promedio_ventas_territorio %>%
  pull(Territorio)
  

##  A mi criterio, debemos dejar de operar en los territorios donde tenemos ventas menores a 1000.
# A comparacion con otros donde las ventas superan el monto de  10,000
```
