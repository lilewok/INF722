library(tidyverse)
library(tidytext)

#Read in file 
mtt <- read_file("data/MyTalkTools/MyTalkTools-About.txt")

#output file to console 
#cat(mtt)

#Remove punctuation and replace with space
#also removes extra white space
mtt <- str_replace_all(mtt, "\\s+", " ") %>% 
  str_replace_all(., "\\p{Punct}", " ")  

#places data into data frame table
mtt_df <- tibble(mtt)

#creating unigrams
tidy_mtt <- mtt_df %>% unnest_tokens(word,mtt, token = "ngrams", n= 3)

#loading dataset called stop_words
data("stop_words")

#include custom stop words
custom_stop_words <- bind_rows(tibble(word = c("app","apps","apple"),
                                      lexicon = c("custom")),
                               stop_words)

#removing all stop words
tidy_mtt <- tidy_mtt %>% anti_join(custom_stop_words)

#word count
tidy_mtt %>% count(word, sort = TRUE) 

#frequency graph
tidy_mtt %>%
  count(word, sort = TRUE) %>%
  filter(n > 1) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


