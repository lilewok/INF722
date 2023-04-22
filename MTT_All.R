library(tidyverse)
library(tidytext)

###Read in files###
    mtt <- read_file("data/MyTalkTools/MyTalkTools-About.txt")
  # set the directory containing the review files
    file_dir <- "data/MyTalkTools/reviews/"
  # get a list of file names in the directory
    file_paths <- list.files(file_dir, full.names = TRUE)
    
###Create files###
    # set the file path for the output file
      output_file <- "data/MyTalkTools/mtt_about.csv"
      output_file2 <- "data/MyTalkTools/mtt_about_2.csv"
      output_file3 <- "data/MyTalkTools/mtt_about_3.csv"
      output_file4 <- "data/MyTalkTools/mtt_reviews.csv"
      output_file5 <- "data/MyTalkTools/mtt_reviews2.csv"
      output_file6 <- "data/MyTalkTools/mtt_reviews3.csv"
      
###Cleanup About Files###
  #Remove punctuation and replace with space
  #also removes extra white space
      mtt <- str_replace_all(mtt, "\\s+", " ") %>% 
        str_replace_all(., "\\p{Punct}", " ")  
  #places about data into data frame table
      mtt_df <- tibble(mtt)
  #loading dataset called stop_words
      data("stop_words")
  #include custom stop words
      custom_stop_words <- bind_rows(tibble(word = c("app","apps","apple","ipad","aac","touchchat","proloquo2go","ve",
                                                     "mytalktools","mytalk","don","too","iphone","day","pts","lot","lot",
                                                     "itunes","it's","i've","ipod","2","000","20","10","3","5"),
                                            lexicon = c("custom")),
                                     stop_words)
###Process Data###      
      tidy_mtt <- mtt_df %>% unnest_tokens(word,mtt) 
      tidy_mtt <- tidy_mtt %>% anti_join(custom_stop_words)
      tidy_mtt2 <- mtt_df %>% unnest_tokens(word,mtt, token = "ngrams", n= 2) 
      tidy_mtt2 <- tidy_mtt2 %>% anti_join(custom_stop_words)
      tidy_mtt3 <- mtt_df %>% unnest_tokens(word,mtt, token = "ngrams", n= 3)
      tidy_mtt3 <- tidy_mtt3 %>% anti_join(custom_stop_words)

  # read in the files and combine the data
    mtt_reviews <- map(file_paths, read_file) %>% 
    # combine the text into a single character vector
      reduce(c) %>% 
    # convert to a data frame
      tibble(text = .) %>% 
    # separate the text into words
      unnest_tokens(word, text)
    # read in the files and combine the data
      mtt_reviews2 <- map(file_paths, read_file) %>% 
    # combine the text into a single character vector
      reduce(c) %>% 
    # convert to a data frame
      tibble(text = .) %>% 
    # separate the text into words
      unnest_tokens(word, text, token = "ngrams", n= 2)

      mtt_reviews3 <- map(file_paths, read_file) %>% 
    # combine the text into a single character vector
      reduce(c) %>% 
    # convert to a data frame
      tibble(text = .) %>% 
    # separate the text into words
      unnest_tokens(word, text, token = "ngrams", n= 3)
  
    #removing all stop words
      mtt_reviews <- mtt_reviews %>% anti_join(custom_stop_words)
      mtt_reviews2 <- mtt_reviews2 %>% anti_join(custom_stop_words)
      mtt_reviews3 <- mtt_reviews3 %>% anti_join(custom_stop_words)
      



###Output Data###
    #word count
      tidy_mtt <- tidy_mtt %>% count(word, sort = TRUE) 
      tidy_mtt2 <-tidy_mtt2 %>% count(word, sort = TRUE) 
      tidy_mtt3 <-tidy_mtt3 %>% count(word, sort = TRUE) 
      mtt_reviews <- mtt_reviews %>% count(word, sort = TRUE) 
      mtt_reviews2 <- mtt_reviews2 %>% count(word, sort = TRUE) 
      mtt_reviews3 <- mtt_reviews3 %>% count(word, sort = TRUE) 

   # write the combined data frame to a CSV file
      write_csv(tidy_mtt, output_file)
      write_csv(tidy_mtt2, output_file2)
      write_csv(tidy_mtt3, output_file3)
      write_csv(mtt_reviews, output_file4)
      write_csv(mtt_reviews2, output_file5)
      write_csv(mtt_reviews3, output_file6)



###Create Graphs###
    #frequency graph
      tidy_mtt %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      tidy_mtt2 %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      tidy_mtt3 %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      mtt_reviews %>%
        count(word, sort = TRUE) %>%
       filter(n > 0) %>%
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

