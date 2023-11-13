
library(DataExplorer)
library(readr)
library(dplyr)


df <- read_delim("cardio_train.csv" , delim = ';')


introduce(df)

df <- df %>% 
  mutate(age = floor(age/365.25))


colSums(is.na(df))


plot_missing(df)


# ver las distribuciones de ciertas variables
plot_histogram(df)

# ver la densidad
plot_density(df)


#ver las correlaciones

plot_correlation(df)

plot_boxplot(df)


create_report(df)
