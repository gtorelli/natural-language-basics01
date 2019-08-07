# NATURAL LANGUAGE BASICS 01
# Read Dante's Inferno book (PDF), apply stopwords, extract information and generate plots
# Author: Gabriel Torelli | torelli@yahoo.com

library(pacman)
pacman::p_load(pdftools, tibble, dplyr, tidytext, readr, 
               tm, wordcloud, stopwords, ggplot2, ggraph, 
               tidyr, widyr, igraph, devtools, topicmodels, magrittr, ldatuning, purrr)

setwd("/textMining/")

stopwords <- read_delim("stopwords.csv", ";", escape_double = FALSE, trim_ws = TRUE)
text <- paste(pdf_text("DantesInferno.pdf")," ")
text <- unlist(strsplit(text,"[.]"))
text <- tibble(line = text) 

text$line <- text$line %>%
  removePunctuation() %>%
  stripWhitespace() %>%
  removeNumbers()

tokens <- text %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word,line) %>%
  anti_join(stopwords)

tokens_count <- tokens %>%
  count(word, sort = TRUE)

wordcloud(tokens_count$word, tokens_count$n, 
          max.words = 200, random.order=FALSE, scale = c(2,0.5), 
          colors = brewer.pal(8, "Dark2"))

tokens_count %>%
  mutate(word = reorder(word,n)) %>%
  head(9) %>%
  ggplot(aes(word,n,fill=factor(word)))+
  scale_fill_brewer(palette="Purples")+
  geom_col()+
  xlab(NULL)+
  coord_flip()

bigrams <- text %>%
  unnest_tokens(bigram, line, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% as.vector(t(stopwords$word))) %>%
  filter(!word2 %in% as.vector(t(stopwords$word))) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE)

bigrams_graph <- bigrams %>%
  separate(bigram, c("word1", 'word2'), sep = " ") %>%
  filter(n>5) %>%
  graph_from_data_frame()

trigrams <- text %>%
  unnest_tokens(trigram, line, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% as.vector(t(stopwords$word))) %>%
  filter(!word3 %in% as.vector(t(stopwords$word))) %>%
  unite(trigram, word1, word2, word3, sep = " ") %>%
  count(trigram, sort = TRUE)

trigrams_graph <- trigrams %>%
  separate(trigram, c("word1", 'word2', 'word3'), sep = " ") %>%
  filter(n>5) %>%
  graph_from_data_frame()

set.seed(2019)
a <- grid::arrow(type = "closed",
                 length = unit(.15, "inches"))
ggraph(bigrams_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
                 show.legend = FALSE,
                 arrow = a,
                 end_cap = circle(.07, "inches"))+
  geom_node_point(color= "lightgreen", size=5)+
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  theme_void()

set.seed(2019)
a <- grid::arrow(type = "closed",
                 length = unit(.15, "inches"))
ggraph(trigrams_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
                 show.legend = FALSE,
                 arrow = a,
                 end_cap = circle(.07, "inches"))+
  geom_node_point(color= "orange", size=5)+
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  theme_void()

#TOPICOS

dtm <- tokens %>%
  count(linenumber, word, sort = TRUE) %>%
  cast_dtm(linenumber, word, n)

corpus_lda <- dtm %>%
  LDA(k=7)

get_terms(corpus_lda, 7)
corpus_topics <-  tidy(corpus_lda, matrix="beta")

# most common topics
corpus_top_terms <- corpus_topics %>%
  group_by(topic) %>%
  top_n(10, beta)  %>%
  arrange(topic, -beta) %>%
  do(head(. , n=10 )) %>%
  ungroup() %>%
  mutate(term=reorder(term,beta)) %>%
  mutate(order = row_number())

corpus_top_terms %>%
  ggplot(aes(order,beta,fill=factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  xlab("Termos") +
  ylab("Beta") +
  scale_x_continuous(
    breaks = corpus_top_terms$order,
    labels = corpus_top_terms$term ,
    expand = c(0,0) ,
    trans = "reverse"
  ) + coord_flip()

findFreqTerms(dtm, lowfreq = 50)

n_topics <- c(3,5,7,10,13)
ap_lda_compare <- n_topics %>%
  map(LDA, x = dtm)

tibble(k = n_topics, perplex = map_dbl(ap_lda_compare, perplexity)) %>%
  ggplot(aes(k, perplex)) +
  geom_point() +
  geom_line() +
  labs(title="LDA Topic Model Fit",
       subtitle =  "Optimal number of topics",
       x = "number of topics",
       y = "Perplexy"
  )

