library(dplyr)
library(tidyverse)
library(highcharter)

df <- read_delim("2010-201-top.csv", ";" , escape_double = FALSE , trim_ws = TRUE)



# ver la estructura del data frame
str(df)
glimpse(df)

## Renombrando columnas
rename(df, top_genre = 'top_genre')

df %>% 
  

### Filtrando variables
head( select(df, artist, year))



### Filtrando usando dplyr
df %>% 
  select(artist,year) %>% 
  head()

### quiero todas las columnas menos 1
df %>% 
  select(-1)

### Texto a factores

df  %>% 
  mutate_if(is.character, as.factor) %>% 
  glimpse()

### FILTRANDO 

df %>% 
  select(artist, title, year) %>% 
  filter(year == 2010)


### Agregaciones: artistas por año
df %>% 
  select(year, artist) %>%  
  group_by(year) %>%
  summarise(artists = n_distinct(artist))%>% 
  filter(artists > 50)


### cuantos artistas tenemos
df %>% 
  summarise(Artistas = n_distinct(artist))..

### que artista han tenido mas de una cancion que fue popular es mas de un año

df %>% 
  group_by(artist,title) %>% 
  count() %>% 
  filter(n > 1) %>% 
  group_by(artist) %>% 
  summarise(artistas = n()) %>% 
  filter(artistas> 1)

