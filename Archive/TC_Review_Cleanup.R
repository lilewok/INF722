library(tidyverse)
library(tidytext)

#Read in file 
tc_reviews <- read.csv("data/TouchChat/tc.csv")

#output file to console 
list(tc_reviews)

#Remove punctuation and replace with space
#also removes extra white space
#tc_reviews <- str_replace_all(tc_reviews, "\\s+", " ")# %>% 
#  str_replace_all(., "\\p{Punct}", " ")  

#places data into data frame table
#tc_reviews_df <- tibble(tc_reviews)

#loading dataset called stop_words
data("stop_words")

#include custom stop words
custom_stop_words <- bind_rows(tibble(word = c("app","apps","apple","ipad","aac","touchchat","proloquo2go","ve",
                                               "mytalktools","mytalk","don","too","iphone","day","pts","lot","lot",
                                               "itunes","it's","i've","ipod","2","000","20","10","3","5"),
                                      lexicon = c("custom")),
                                      stop_words)

#removing all stop words
tc_reviews <- tc_reviews %>% anti_join(custom_stop_words)

#word count
tc_reviews %>% count(word, sort = TRUE) 

#frequency graph
tc_reviews %>%
  count(word, sort = TRUE) %>%
  filter(n > 1) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

