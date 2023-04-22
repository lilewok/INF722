library(tidyverse)
library(tidytext)

# set the directory containing the files
file_dir <- "data/MyTalkTools/reviews/"

# get a list of file names in the directory
file_paths <- list.files(file_dir, full.names = TRUE)

#loading dataset called stop_words
data("stop_words")

# read in the files and combine the data
mtt_reviews <- map(file_paths, read_file) %>% 
  # combine the text into a single character vector
  reduce(c) %>% 
  # convert to a data frame
  tibble(text = .) %>% 
  # separate the text into words
  unnest_tokens(word, text)

# set the file path for the output file
output_file <- "data/MyTalkTools/mtt.csv"

# write the combined data frame to a CSV file
write_csv(mtt_reviews, output_file)

# read in the files and combine the data
mtt_reviews2 <- map(file_paths, read_file) %>% 
  # combine the text into a single character vector
  reduce(c) %>% 
  # convert to a data frame
  tibble(text = .) %>% 
  # separate the text into words
  unnest_tokens(word, text, token = "ngrams", n= 2)

# set the file path for the output file
output_file2 <- "data/MyTalkTools/mtt2.csv"

# write the combined data frame to a CSV file
write_csv(mtt_reviews2, output_file2)

# read in the files and combine the data
mtt_reviews3 <- map(file_paths, read_file) %>% 
  # combine the text into a single character vector
  reduce(c) %>% 
  # convert to a data frame
  tibble(text = .) %>% 
  # separate the text into words
  unnest_tokens(word, text, token = "ngrams", n= 3)

# set the file path for the output file
output_file3 <- "data/MyTalkTools/mtt3.csv"

# write the combined data frame to a CSV file
write_csv(mtt_reviews3, output_file3)


#include custom stop words
custom_stop_words <- bind_rows(tibble(word = c("app","apps","apple","ipad","aac","MyTalkTools","proloquo2go","ve",
                                               "mytalktools","mytalk","don","too","iphone","day","pts","lot","lot",
                                               "itunes","it's","i've","ipod","2","000","20","10","3","5"),
                                      lexicon = c("custom")),
                               stop_words)

#removing all stop words
mtt_reviews <- mtt_reviews %>% anti_join(custom_stop_words)
mtt_reviews2 <- mtt_reviews2 %>% anti_join(custom_stop_words)
mtt_reviews3 <- mtt_reviews3 %>% anti_join(custom_stop_words)

#word count
mtt_reviews %>% count(word, sort = TRUE) 
mtt_reviews2 %>% count(word, sort = TRUE) 
mtt_reviews3 %>% count(word, sort = TRUE) 


#frequency graph
mtt_reviews %>%
  count(word, sort = TRUE) %>%
 filter(n > 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
mtt_reviews2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 0) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
mtt_reviews3 %>%
  count(word, sort = TRUE) %>%
  filter(n > 0) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

