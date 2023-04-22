library(tidyverse)
library(tidytext)

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
      tidy_tc <- tc_df %>% unnest_tokens(word,tc) 
      tidy_tc <- tidy_tc %>% anti_join(custom_stop_words)
      tidy_tc2 <- tc_df %>% unnest_tokens(word,tc, token = "ngrams", n= 2) 
      tidy_tc2 <- tidy_tc2 %>% anti_join(custom_stop_words)
      tidy_tc3 <- tc_df %>% unnest_tokens(word,tc, token = "ngrams", n= 3)
      tidy_tc3 <- tidy_tc3 %>% anti_join(custom_stop_words)

  # read in the files and combine the data
    tc_reviews <- map(file_paths, read_file) %>% 
    # combine the text into a single character vector
      reduce(c) %>% 
    # convert to a data frame
      tibble(text = .) %>% 
    # separate the text into words
      unnest_tokens(word, text)
    # read in the files and combine the data
      tc_reviews2 <- map(file_paths, read_file) %>% 
    # combine the text into a single character vector
      reduce(c) %>% 
    # convert to a data frame
      tibble(text = .) %>% 
    # separate the text into words
      unnest_tokens(word, text, token = "ngrams", n= 2)

      tc_reviews3 <- map(file_paths, read_file) %>% 
    # combine the text into a single character vector
      reduce(c) %>% 
    # convert to a data frame
      tibble(text = .) %>% 
    # separate the text into words
      unnest_tokens(word, text, token = "ngrams", n= 3)
  
    #removing all stop words
      tc_reviews <- tc_reviews %>% anti_join(custom_stop_words)
      tc_reviews2 <- tc_reviews2 %>% anti_join(custom_stop_words)
      tc_reviews3 <- tc_reviews3 %>% anti_join(custom_stop_words)
      



###Output Data###
    #word count
      tidy_tc <- tidy_tc %>% count(word, sort = TRUE) 
      tidy_tc2 <-tidy_tc2 %>% count(word, sort = TRUE) 
      tidy_tc3 <-tidy_tc3 %>% count(word, sort = TRUE) 
      tc_reviews <- tc_reviews %>% count(word, sort = TRUE) 
      tc_reviews2 <- tc_reviews2 %>% count(word, sort = TRUE) 
      tc_reviews3 <- tc_reviews3 %>% count(word, sort = TRUE) 

   # write the combined data frame to a CSV file
      write_csv(tidy_tc, output_file)
      write_csv(tidy_tc2, output_file2)
      write_csv(tidy_tc3, output_file3)
      write_csv(tc_reviews, output_file4)
      write_csv(tc_reviews2, output_file5)
      write_csv(tc_reviews3, output_file6)



###Create Graphs###
    #frequency graph
      tidy_tc %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      tidy_tc2 %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      tidy_tc3 %>%
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

