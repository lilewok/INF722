library(tidyverse)
library(tidytext)

#Read in file 
mtt <- read_file("data/MyTalkTools/MyTalkTools-About.txt")


cat(mtt)

mtt <- str_replace_all(mtt, "\\s+", " ") %>% # Remove extra white spaces
  str_replace_all(., "\\p{Punct}", "")  # Remove punctuations



mtt_df <- tibble(mtt)

tidy_mtt <- mtt_df %>% unnest_tokens(word,mtt)

str(mtt_df)

data("stop_words")

custom_stop_words <- bind_rows(tibble(word = c("app","apps"),
                                      lexicon = c("custom")),
                               stop_words)

tidy_mtt <- tidy_mtt %>% anti_join(custom_stop_words)

tidy_mtt %>% count(word, sort = TRUE) 

tidy_mtt %>%
  count(word, sort = TRUE) %>%
  filter(n > 1) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +n 
  labs(y = NULL)


