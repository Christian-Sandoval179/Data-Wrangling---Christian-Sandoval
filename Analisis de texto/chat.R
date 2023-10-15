library(lubridate)
library(rwhatsapp)
library(textdata)
library(tidytext)
library(tidyverse)
library(tidyr)
library(ggimage)
library(stopwords)
library(ggplot2)
library(igraph)
library(ggraph)
library(dplyr)


chat <- rwa_read("chat.txt")

chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity", fill="lightblue") +
  ylab("Frecuencia") + xlab("Fecha") +
  ggtitle("Mensajes por día")


#### mensajes por hora y frecuencia de dia
diasemana <- c("domingo","lunes","martes","miércoles","jueves","viernes","sábado","domingo")
names(diasemana) <- 1:7# MENSAJES POR HORA DEL DÍA
chat %>%
  mutate( day = date(time),
          wday.num = wday(day),
          wday.name = weekdays(day),
          hour = hour(time)) %>%
  count(wday.num, wday.name,hour) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_bar(stat = "identity") +
  ylab("Número de mensajes") + xlab("Horario") +
  ggtitle("Número de mensajes por hora del día") +
  facet_wrap(~wday.num, ncol=7, labeller = labeller(wday.num=diasemana))+
  theme_minimal() +
  theme( legend.title = element_blank(),
         legend.position = "bottom",
         axis.text.x = element_text(angle = 90))


        
        chat %>%
          mutate(day = date(time)) %>%
          count(author) %>%
          ggplot(aes(x = reorder(author, n), y = n)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = scales::comma(n)), hjust = -0.1) +
          ylab("Frecuencia") + xlab("Sender") +
          coord_flip() +
          ggtitle("Cantidad de mensajes enviados")
        
        
        ## Palabras mas frecuentes
        chat %>%
          unnest_tokens(input = text,
                        output = word) %>%
          select(word, author) %>%
          mutate(word = gsub(".com", "", word)) %>%
          count(author, word, sort = TRUE) %>%
          filter(n > 10) %>%
          group_by(author) %>%
          top_n(n = 6) %>%
          ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
          geom_col(show.legend = FALSE) +
          geom_text(aes(label = scales::comma(n)), hjust = -0.1) +
          ylab("") +
          xlab("") +
          coord_flip() +
          facet_wrap(~author, ncol = 2, scales = "free_y") +
          scale_x_reordered() +
          ggtitle("Palabras mas frecuentes por sender")
        
        ## Stopwords
        
        
        to_remove <- c(stopwords(language = "en"),
                       stopwords(language = "es"),
                       c(1:1000),
                       c("imagen"
                         ,"sticker"
                         ,"image"
                         ,"omitida"
                         ,"omitted"
                         ,"jajaja"
                         ,"jajajaja"))
        chat %>%
          
        unnest_tokens(input = text,
                      output = word) %>%
          select(word, author) %>%
          filter(!word %in% to_remove) %>%
          mutate(word = gsub(".com", "", word)) %>%
          count(author, word, sort = TRUE) %>%
          filter(n > 10) %>%
          group_by(author) %>%
          top_n(n = 6) %>%
          ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
          geom_col(show.legend = FALSE) +
          ylab("") +
          xlab("") +
          coord_flip() +
          facet_wrap(~author, ncol = 2, scales = "free_y") +
          scale_x_reordered() +
          ggtitle("Palabras mas frecuentes por sender descontando stopwords")
        
        
        ## Diversidad lexica
        
        chat %>%
          unnest_tokens(input = text,
                        output = word) %>%
          filter(!word %in% to_remove) %>%
          group_by(author) %>%
          summarise(lex_diversity = n_distinct(word)) %>%
          arrange(desc(lex_diversity)) %>%
          ggplot(aes(x = reorder(author, lex_diversity),
                     y = lex_diversity,
                     fill = author)) +
          geom_col(show.legend = FALSE) +
          scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
          geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
          ylab("Palabras unicas") +
          xlab("") +
          ggtitle("Diversidad de palabras") +
          coord_flip()
        
        ## palabras unicas por sender
        
        o_words <- chat %>%
          unnest_tokens(input = text,
                        output = word) %>%
          filter(author != unique(chat$author)[1]) %>%
          count(word, sort = TRUE)
        chat %>%
          unnest_tokens(input = text,
                        output = word) %>%
          filter(author == unique(chat$author)[1]) %>%
          filter(!word %in% to_remove) %>%
          count(word, sort = TRUE) %>%
          filter(!word %in% o_words$word) %>% # only select words nobody else uses
          top_n(n = 6, n) %>%
          ggplot(aes(x = reorder(word, n), y = n)) +
          geom_col(show.legend = FALSE) +
          ylab("") + xlab("") +
          coord_flip() +
          ggtitle(paste("Palabras únicas", unique(chat$author)[1]))
        
        
        ## TF-IDF
        
        chat %>%
          unnest_tokens(input = text,
                        output = word) %>%
          select(word, author) %>%
          filter(!word %in% to_remove) %>%
          count(author, word, sort = TRUE) %>%
          bind_tf_idf(term = word, document = author, n = n) %>%
          filter(n > 10) %>%
          group_by(author) %>%
          top_n(n = 6, tf_idf) %>%
          ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
          geom_col(show.legend = FALSE) +
          ylab("") +
          xlab("") +
          coord_flip() +
          facet_wrap(~author, ncol = 2, scales = "free_y") +
          scale_x_reordered() +
          ggtitle("Palabras utilizando tf–idf por autor")
        
        
        
        paired_words <- chat %>%
          filter(author==unique(chat$author)[2]) %>%
          select(text) %>%
          unnest_tokens(paired_words,text, token = "ngrams", n =2)
        paired_separated_words <- paired_words %>%
          separate(paired_words, c("word1", "word2"), sep = " ")
        paired_separated_words_filtered <- paired_separated_words %>%
          filter(!word1 %in% to_remove) %>%
          filter(!word2 %in% to_remove)
        # new bigram counts:
        paired_words_counts <- paired_separated_words_filtered %>%
          count(word1, word2, sort = TRUE)
        paired_words_counts %>% filter(n >= 3) %>%
          graph_from_data_frame() %>%
          ggraph(layout = "fr") +
          geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
          geom_node_point(color = "darkslategray4", size = 3) +
          geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
          labs(title = "Word Network",
               subtitle = "Whatsapp Chats ",
               x = "", y = "")
        
        
        
        
        
        