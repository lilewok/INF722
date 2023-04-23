library(tidyverse)
library(tidytext)
library(scales)


###Read in files###
    tc <- read_file("data/TouchChat/TouchChat-About.txt")
  # set the directory containing the review files
    file_dir <- "data/TouchChat/reviews/"
  # get a list of file names in the directory
    file_paths <- list.files(file_dir, full.names = TRUE)
    
###Create files###
    # set the file path for the output file
      output_file <- "data/TouchChat/tc_about.csv"
      output_file2 <- "data/TouchChat/tc_about_2.csv"
      output_file3 <- "data/TouchChat/tc_about_3.csv"
      output_file4 <- "data/TouchChat/tc_reviews.csv"
      output_file5 <- "data/TouchChat/tc_reviews2.csv"
      output_file6 <- "data/TouchChat/tc_reviews3.csv"
      output_file_test <- "data/Test.csv"
      
###Cleanup About Files###
  #Remove punctuation and replace with space
  #also removes extra white space
      tc <- str_replace_all(tc, "\\s+", " ") %>% 
        str_replace_all(., "\\p{Punct}", " ")  
  #places about data into data frame table
      tc_df <- tibble(tc)
  #loading dataset called stop_words
      data("stop_words")
  #include custom stop words
      custom_stop_words <- bind_rows(tibble(word = c("app","apps","apple","ipad","aac","touchchat","TouchChat","ve",
                                                     "TouchChat","mytalk","don","too","iphone","day","pts","lot","lot",
                                                     "itunes","it's","i've","ipod","2","000","20","10","3","5"),
                                            lexicon = c("custom")),
                                     stop_words)
###Process Data###    
  ## Create "about" ngrams
      tc_about <- tc_df %>% unnest_tokens(word,tc) 
      tc_about <- tc_about %>% anti_join(custom_stop_words)
      tc_about2 <- tc_df %>% unnest_tokens(word,tc, token = "ngrams", n= 2) 
      tc_about2 <- tc_about2 %>% anti_join(custom_stop_words)
      tc_about3 <- tc_df %>% unnest_tokens(word,tc, token = "ngrams", n= 3)
      tc_about3 <- tc_about3 %>% anti_join(custom_stop_words)

  # combine data and create "review" ngrams
    tc_reviews <- map(file_paths, read_file) %>% 
    # combine the text into a single character vector
      reduce(c) %>% 
    # convert to a data frame
      tibble(text = .) %>% 
    # separate the text into unigrams
      unnest_tokens(word, text)
    # read in the files and combine the data
      tc_reviews2 <- map(file_paths, read_file) %>% 
    # combine the text into a single character vector
      reduce(c) %>% 
    # convert to a data frame
      tibble(text = .) %>% 
    # separate the text into bigrams
      unnest_tokens(word, text, token = "ngrams", n= 2)
      tc_reviews3 <- map(file_paths, read_file) %>% 
    # combine the text into a single character vector
      reduce(c) %>% 
    # convert to a data frame
      tibble(text = .) %>% 
    # separate the text into trigrams
      unnest_tokens(word, text, token = "ngrams", n= 3)
  
    #removing all stop words
      tc_reviews <- tc_reviews %>% anti_join(custom_stop_words)
      tc_reviews2 <- tc_reviews2 %>% anti_join(custom_stop_words)
      tc_reviews3 <- tc_reviews3 %>% anti_join(custom_stop_words)
      



###Output Data###
    #word count
      tc_about <- tc_about %>% count(word, sort = TRUE) 
      tc_about2 <-tc_about2 %>% count(word, sort = TRUE) 
      tc_about3 <-tc_about3 %>% count(word, sort = TRUE) 
      tc_reviews <- tc_reviews %>% count(word, sort = TRUE) 
      tc_reviews2 <- tc_reviews2 %>% count(word, sort = TRUE) 
      tc_reviews3 <- tc_reviews3 %>% count(word, sort = TRUE) 

   # write the combined data frame to a CSV file
      write_csv(tc_about, output_file)
      write_csv(tc_about2, output_file2)
      write_csv(tc_about3, output_file3)
      write_csv(tc_reviews, output_file4)
      write_csv(tc_reviews2, output_file5)
      write_csv(tc_reviews3, output_file6)
  
  # frequency matrices
      freq_matrix <- table(tc_about$word, tc_about$n)
      print(freq_matrix)
      freq_matrix <- table(tc_about2$word, tc_about2$n)
      print(freq_matrix)
      freq_matrix <- table(tc_about3$word, tc_about3$n)
      print(freq_matrix)
      freq_matrix <- table(tc_reviews$word, tc_reviews$n)
      print(freq_matrix)
      freq_matrix <- table(tc_reviews2$word, tc_reviews2$n)
      print(freq_matrix)
      freq_matrix <- table(tc_reviews3$word, tc_reviews3$n)
      print(freq_matrix)



###Create Graphs###
    #frequency graphs
      tc_about %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      tc_about2 %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      tc_about3 %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      tc_reviews %>%
        count(word, sort = TRUE) %>%
       filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      tc_reviews2 %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      tc_reviews3 %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      
###Test Area###
      
      frequency <- bind_rows(mutate(tc_about, product = "MyTalkTools"),
                             mutate(qt_about, product = "QuickTalk"), 
                             mutate(mtt_about, product = "TouchChat"),
                             mutate(plq2g_about, product = "Proloquo2go")) %>% 
        mutate(word = str_extract(word, "[a-z']+")) %>%
        count(product, word) %>%
        group_by(product) %>%
        mutate(proportion = n / sum(n)) %>% 
        select(-n) %>% 
        pivot_wider(names_from = product, values_from = proportion) %>%
        pivot_longer(`TouchChat`:`QuickTalk`:`MyTalkTools`:`Proloquo2go`,
                     names_to = "product", values_to = "proportion")

print(frequency)      
write_csv(frequency, output_file_test)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `MyTalkTools`, 
                      color = abs(`MyTalkTools` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~product, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "MyTalkTools", x = NULL)

