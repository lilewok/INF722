library(tidyverse)
library(tidytext)

#Read in file 
plq2g_reviews <- read.csv("data/Proloquo2go/plq2g.csv")

#output file to console 
list(plq2g_reviews)

#Remove punctuation and replace with space
#also removes extra white space
#plq2g_reviews <- str_replace_all(plq2g_reviews, "\\s+", " ")# %>% 
#  str_replace_all(., "\\p{Punct}", " ")  

#places data into data frame table
#plq2g_reviews_df <- tibble(plq2g_reviews)

#loading dataset called stop_words
data("stop_words")

#include custom stop words
custom_stop_words <- bind_rows(tibble(word = c("app","apps","apple","ipad","aac","touchchat","proloquo2go","ve",
                                               "mytalktools","mytalk","don","too","iphone","day","pts","lot","lot",
                                               "itunes","it's","i've","ipod","2","000","20","10","3","5"),
                                      lexicon = c("custom")),
                                      stop_words)
#removing all stop words
plq2g_reviews <- plq2g_reviews %>% anti_join(custom_stop_words)

#word count
plq2g_reviews %>% count(word, sort = TRUE) 

#frequency graph
plq2g_reviews %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

