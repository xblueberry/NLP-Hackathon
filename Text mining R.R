#libraries
library(XML) #read XML
library(tidytext)
library(tidyr)
library(dplyr)
library(tm) #dutch stopwords
library(ggplot2)
library(wordcloud) #represent count plot
library(igraph); library(ggraph) #plot bigrams
library(topicmodels) #LDA


#import dataset
url <- "C:/Users/Daniel/Desktop/Hackathon/VLAIO_dataset_Projecten geklasseerd in topics_v20181008_2027.XML"
df <- xmlToDataFrame(url)

df$topic_x0020_Energie<-as.factor(df$topic_x0020_Energie)
df$topic_x0020_AI<-as.factor(df$topic_x0020_AI)
summary(df)

#Work with just the text for now...
df<-df[,-2]


############################### tokenize #################################

tidy_df <- df %>%
  unnest_tokens(word, pj_samenvatting)

#stopwords dutch and english
data(stop_words)

stop_words_dutch<-as.data.frame(stopwords(kind = "dutch"))
colnames(stop_words_dutch)[1] <- "word"

tidy_df <- tidy_df %>%
  anti_join(stop_words_dutch,by="word") %>%
  anti_join(stop_words)

#count
tidy_df %>%
  count(word, sort = TRUE)


#plot
tidy_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 1500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



tidy_df %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


#by category of energy
unigram_tf_idf <- tidy_df %>%
  count(topic_x0020_Energie, word) %>%
  bind_tf_idf(word, topic_x0020_Energie, n) %>%
  arrange(desc(tf_idf))
unigram_tf_idf

#clean...
book_words <- df %>%
  unnest_tokens(word, pj_samenvatting) %>%
  count(topic_x0020_Energie, word, sort = TRUE) %>%
  ungroup()

book_words <- book_words %>%
  bind_tf_idf(word, topic_x0020_Energie, n)%>%
  arrange(desc(tf_idf))
book_words

#plot
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(topic_x0020_Energie) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = topic_x0020_Energie)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~topic_x0020_Energie, ncol = 2, scales = "free") +
  coord_flip()



############################### bigrams ##########################################

df_bigrams <- df %>%
  unnest_tokens(bigram, pj_samenvatting, token = "ngrams", n = 2)

df_bigrams %>%
  count(bigram, sort = TRUE)

#without stopwords
bigrams_separated <- df_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words_dutch$word) %>%
  filter(!word2 %in% stop_words_dutch$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
# new bigram counts:
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united


#by category of energy
bigram_tf_idf <- bigrams_united %>%
  count(topic_x0020_Energie, bigram) %>%
  bind_tf_idf(bigram, topic_x0020_Energie, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf


#network of bigrams

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 50) %>%
  graph_from_data_frame()
bigram_graph

set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#fancier..
set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



################################# LDA ##################################

tidy_df<-tidy_df %>%
  count(pj_aionummer,word, sort = TRUE) %>%
  ungroup()

desc_dtm <- tidy_df %>%
  cast_dtm(pj_aionummer,word, n)

#(30min for k=10)
desc_lda <- LDA(desc_dtm, k = 10, control = list(seed = 1234))
desc_lda

tidy_lda <- tidy(desc_lda)
tidy_lda

#top terms by topic
top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"),
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 3, scales = "free")



lda_gamma <- tidy(desc_lda, matrix = "gamma")
lda_gamma

lda_gamma <- full_join(lda_gamma, topic_x0020_Energie, by = c("document" = "id"))
lda_gamma