library(tidyverse)
library(tidytext)

#Read in file 
qt <- read_file("data/QuickTalk/QuickTalk-About.txt")

#output file to console 
#cat(qt)

#Remove punctuation and replace with space
#also removes extra white space
qt <- str_replace_all(qt, "\\s+", " ") %>% 
  str_replace_all(., "\\p{Punct}", " ")  

#places data into data frame table
qt_df <- tibble(qt)

#creating unigrams
tidy_qt <- qt_df %>% unnest_tokens(word,qt)

#loading dataset called stop_words
data("stop_words")

#include custom stop words
custom_stop_words <- bind_rows(tibble(word = c("app","apps","apple","ipad","aac","touchchat","proloquo2go","ve",
                                               "mytalktools","mytalk","don","too","iphone","day","pts","lot","lot",
                                               "itunes","it's","i've","ipod","2","000","20","10","3","5"),
                                      lexicon = c("custom")),
                                      stop_words)

#removing all stop words
tidy_qt <- tidy_qt %>% anti_join(custom_stop_words)

#word count
tidy_qt %>% count(word, sort = TRUE) 

#frequency graph
tidy_qt %>%
  count(word, sort = TRUE) %>%
  filter(n > 1) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
