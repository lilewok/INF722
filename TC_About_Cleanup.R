library(tidyverse)
library(tidytext)

#Read in file 
tc <- read_file("data/TouchChat/TouchChat-About.txt")

#output file to console 
#cat(tc)

#Remove punctuation and replace with space
#also removes extra white space
tc <- str_replace_all(tc, "\\s+", " ") %>% 
  str_replace_all(., "\\p{Punct}", " ")  

#places data into data frame table
tc_df <- tibble(tc)

#creating unigrams
tidy_tc <- tc_df %>% unnest_tokens(word,tc)

#loading dataset called stop_words
data("stop_words")

#include custom stop words
custom_stop_words <- bind_rows(tibble(word = c("app","apps","apple"),
                                      lexicon = c("custom")),
                               stop_words)

#removing all stop words
tidy_tc <- tidy_tc %>% anti_join(custom_stop_words)

#word count
tidy_tc %>% count(word, sort = TRUE) 

#frequency graph
tidy_tc %>%
  count(word, sort = TRUE) %>%
  filter(n > 1) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

