library(tidyverse)
library(tidytext)

#Read in file 
mtt_reviews <- read.csv("data/MyTalkTools/output.csv")

#output file to console 
list(mtt_reviews)

#Remove punctuation and replace with space
#also removes extra white space
#mtt_reviews <- str_replace_all(mtt_reviews, "\\s+", " ")# %>% 
#  str_replace_all(., "\\p{Punct}", " ")  

#places data into data frame table
mtt_reviews_df <- tibble(mtt_reviews)

#creating unigrams
tidy_mtt_reviews <- mtt_reviews_df %>% unnest_tokens(word,mtt_reviews)

#loading dataset called stop_words
data("stop_words")

#include custom stop words
custom_stop_words <- bind_rows(tibble(word = c("app","apps","apple"),
                                      lexicon = c("custom")),
                               stop_words)

#removing all stop words
tidy_mtt_reviews <- tidy_mtt_reviews %>% anti_join(custom_stop_words)

#word count
tidy_mtt_reviews %>% count(word, sort = TRUE) 

#frequency graph
tidy_mtt_reviews %>%
  count(word, sort = TRUE) %>%
  filter(n > 1) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

