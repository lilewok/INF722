library(tidyverse)
library(tidytext)

#Read in file 
ec <- read_file("data/EntireCorpus.txt")

#output file to console 
#cat(mtt)

#Remove punctuation and replace with space
#also removes extra white space
ec <- str_replace_all(ec, "\\s+", " ") %>% 
  str_replace_all(., "\\p{Punct}", " ")  

#places data into data frame table
ec_df <- tibble(ec)

#creating unigrams
tidy_ec <- ec_df %>% unnest_tokens(word,ec)

#loading dataset called stop_words
data("stop_words")

#include custom stop words
custom_stop_words <- bind_rows(tibble(word = c("app","apps","apple","ipad","aac","touchchat","proloquo2go","ve",
                                               "mytalktools","mytalk","don","too","iphone","day","pts","lot","lot",
                                               "itunes","it's","i've","ipod","2","000","20","10","3","5"),
                                      lexicon = c("custom")),
                                      stop_words)

#removing all stop words
tidy_ec <- tidy_ec %>% anti_join(custom_stop_words)

#word count
tidy_ec %>% count(word, sort = TRUE) 

#frequency graph
tidy_ec %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
