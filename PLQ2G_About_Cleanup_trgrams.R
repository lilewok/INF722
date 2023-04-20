library(tidyverse)
library(tidytext)

#Read in file 
plq2g <- read_file("data/Proloquo2go/Proloquo2go-About.txt")

#output file to console 
#cat(plq2g)

#Remove punctuation and replace with space
#also removes extra white space
plq2g <- str_replace_all(plq2g, "\\s+", " ") %>% 
  str_replace_all(., "\\p{Punct}", " ")  

#places data into data frame table
plq2g_df <- tibble(plq2g)

#creating unigrams
tidy_plq2g <- plq2g_df %>% unnest_tokens(word,plq2g, token = "ngrams", n= 3)

#loading dataset called stop_words
data("stop_words")

#include custom stop words
custom_stop_words <- bind_rows(tibble(word = c("app","apps","apple"),
                                      lexicon = c("custom")),
                               stop_words)

#removing all stop words
tidy_plq2g <- tidy_plq2g %>% anti_join(custom_stop_words)

#word count
tidy_plq2g %>% count(word, sort = TRUE) 

#frequency graph
tidy_plq2g %>%
  count(word, sort = TRUE) %>%
  filter(n > 1) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
