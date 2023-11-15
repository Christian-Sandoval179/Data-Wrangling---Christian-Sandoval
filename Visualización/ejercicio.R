library(DataExplorer)
library(readr)
library(dplyr)
library(ggplot2)

data <- read_csv("StudentsPerformance.csv")

data <- data %>%
  rename(math_score = 'math score', reading_score = `reading score` , writing_score = `writing score` )

introduce(data)

plot_missing(data)

plot_histogram(data)


plot_correlation(data)

# Veo las diferentes dietas que hay
unique(data$lunch)

##
data_arriba <- data %>%
  filter(math_score > 80, reading_score > 80, writing_score > 80)
  

## Separamos las notas de los estudiantes de acuerdo a la dieta del estudiante
data_standard <- data %>% 
  filter(lunch == 'standard')

nrow(data_standard)


data_free_reduce <- data %>% 
  filter(lunch == 'free/reduced')

nrow(data_free_reduce)


nrow(data)

porcentaje_standard <- (nrow(data_standard) / nrow(data)) *100
porcentaje_free_reduced <- (nrow(data_free_reduce) / nrow(data)) * 100




# Estudiantes con lunch standard con notas arriba de 80
data_standard_arriba <- data_standard %>% 
  filter(math_score > 80, reading_score > 80, writing_score > 80)

porcentaje_80_free <- (11/355) * 100

# Estudiantes con lunch free/reduced con notas arriba de 80
data_free_arriba <- data_free_reduce %>% 
  filter(math_score > 80, reading_score > 80, writing_score > 80)

porcentaje_80_standard <- (99/645) * 100


summary_by_lunch <- data %>%
  group_by(lunch) %>%
  summarize(
    mean_math_score = mean(math_score),
    mean_reading_score = mean(reading_score),
    mean_writing_score = mean(writing_score)
  )


avg_scores_long <- summary_by_lunch %>%
  pivot_longer(cols = starts_with("mean"), names_to = "Score", values_to = "Mean_Score")

# Crea el gráfico de barras
ggplot(avg_scores_long, aes(x = lunc, y = Mean_Score, fill = Score)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Promedio de Puntuaciones por Tipo de Dieta",
       x = "Tipo de Dieta",
       y = "Promedio de Puntuación") +
  scale_fill_manual(values = c("mean_math_score" = "red", "mean_reading_score" = "blue", "mean_writing_score" = "green")) +
  theme_minimal()

# ANOVA para math_score por tipo de dieta
anova_math <- aov(math_score ~ lunch, data = data)

# ANOVA para reading_score por tipo de dieta
anova_reading <- aov(reading_score ~ lunch, data = data)

# ANOVA para writing_score por tipo de dieta
anova_writing <- aov(writing_score ~ lunch, data = data)


  print("ANOVA for Math Score:")
  print(summary(anova_math))
  
  print("\nANOVA for Reading Score:")
  print(summary(anova_reading))
  
  print("\nANOVA for Writing Score:")
  print(summary(anova_writing))