


print("Hello world")


str(df)
names(df)
head()

nrow()
ncol()

is.na(df)

colSums(is.na())

df[!is.na(df$col3),]

find_mean <- function(x){
  
  return(sum(x)/length(x))
  
}
vec <- 1:10
find_mean(vec)

data <- data.frame(
  a = sample(letters, size = 10, replace = TRUE),
  b = sample(1:10, size = 10, replace =  TRUE)

)

list1 = lapply(1:20000,generate_df)

install.packages("tidyverse")
library(readxl)
library(readr)
library(tidyverse)
library(tidytext)



excel <- readxl:: read_excel('ejemplo1.xlsx')
head(excel)
str(excel)

excel_sheets('ejemplo.xlsx')

excel_2 <- read_excel('ejemplo1.xlsx', sheet = "Sheet1")



install.packages("dplyr")
install.packages("RMySQL")
install.packages("lubridate")
install.packages("openxlsx")
install.packages("tidyverse")
install.packages("stringr")
install.packages("readr")
install.packages("ggplor2")

