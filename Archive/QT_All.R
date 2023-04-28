library(tidyverse)
library(tidytext)

###Read in files###
    qt <- read_file("data/QuickTalk/QuickTalk-About.txt")
  # set the directory containing the review files
    file_dir <- "data/QuickTalk/reviews/"
  # get a list of file names in the directory
    file_paths <- list.files(file_dir, full.names = TRUE)
    
###Create files###
    # set the file path for the output file
      output_file <- "data/QuickTalk/qt_about.csv"
      output_file2 <- "data/QuickTalk/qt_about_2.csv"
      output_file3 <- "data/QuickTalk/qt_about_3.csv"
      output_file4 <- "data/QuickTalk/qt_reviews.csv"
      output_file5 <- "data/QuickTalk/qt_reviews2.csv"
      output_file6 <- "data/QuickTalk/qt_reviews3.csv"
      
###Cleanup About Files###
  #Remove punctuation and replace with space
  #also removes extra white space
      qt <- str_replace_all(qt, "\\s+", " ") %>% 
        str_replace_all(., "\\p{Punct}", " ")  
  #places about data into data frame table
      qt_df <- tibble(qt)
  #loading dataset called stop_words
      data("stop_words")
  #include custom stop words
      custom_stop_words <- bind_rows(tibble(word = c("app","apps","apple","ipad","aac","QuickTalk","QuickTalk","ve",
                                                     "QuickTalk","mytalk","don","too","iphone","day","pts","lot","lot",
                                                     "itunes","it's","i've","ipod","2","000","20","10","3","5"),
                                            lexicon = c("custom")),
                                     stop_words)
###Process Data###      
      qt_about <- qt_df %>% unnest_tokens(word,qt) 
      qt_about <- qt_about %>% anti_join(custom_stop_words)
      qt_about2 <- qt_df %>% unnest_tokens(word,qt, token = "ngrams", n= 2) 
      qt_about2 <- qt_about2 %>% anti_join(custom_stop_words)
      qt_about3 <- qt_df %>% unnest_tokens(word,qt, token = "ngrams", n= 3)
      qt_about3 <- qt_about3 %>% anti_join(custom_stop_words)

  # read in the files and combine the data
    qt_reviews <- map(file_paths, read_file) %>% 
    # combine the text into a single character vector
      reduce(c) %>% 
    # convert to a data frame
      tibble(text = .) %>% 
    # separate the text into words
      unnest_tokens(word, text)
    # read in the files and combine the data
      qt_reviews2 <- map(file_paths, read_file) %>% 
    # combine the text into a single character vector
      reduce(c) %>% 
    # convert to a data frame
      tibble(text = .) %>% 
    # separate the text into words
      unnest_tokens(word, text, token = "ngrams", n= 2)

      qt_reviews3 <- map(file_paths, read_file) %>% 
    # combine the text into a single character vector
      reduce(c) %>% 
    # convert to a data frame
      tibble(text = .) %>% 
    # separate the text into words
      unnest_tokens(word, text, token = "ngrams", n= 3)
  
    #removing all stop words
      qt_reviews <- qt_reviews %>% anti_join(custom_stop_words)
      qt_reviews2 <- qt_reviews2 %>% anti_join(custom_stop_words)
      qt_reviews3 <- qt_reviews3 %>% anti_join(custom_stop_words)
      



###Output Data###
    #word count
      qt_about <- qt_about %>% count(word, sort = TRUE) 
      qt_about2 <-qt_about2 %>% count(word, sort = TRUE) 
      qt_about3 <-qt_about3 %>% count(word, sort = TRUE) 
      qt_reviews <- qt_reviews %>% count(word, sort = TRUE) 
      qt_reviews2 <- qt_reviews2 %>% count(word, sort = TRUE) 
      qt_reviews3 <- qt_reviews3 %>% count(word, sort = TRUE) 

   # write the combined data frame to a CSV file
      write_csv(qt_about, output_file)
      write_csv(qt_about2, output_file2)
      write_csv(qt_about3, output_file3)
      write_csv(qt_reviews, output_file4)
      write_csv(qt_reviews2, output_file5)
      write_csv(qt_reviews3, output_file6)

  # frequency matrices
      freq_matrix <- table(qt_about$word, qt_about$n)
      print(freq_matrix)
      freq_matrix <- table(qt_about2$word, qt_about2$n)
      print(freq_matrix)
      freq_matrix <- table(qt_about3$word, qt_about3$n)
      print(freq_matrix)
      freq_matrix <- table(qt_reviews$word, qt_reviews$n)
      print(freq_matrix)
      freq_matrix <- table(qt_reviews2$word, qt_reviews2$n)
      print(freq_matrix)
      freq_matrix <- table(qt_reviews3$word, qt_reviews3$n)
      print(freq_matrix)
      

###Create Graphs###
    #frequency graph
      qt_about %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      qt_about2 %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      qt_about3 %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      qt_reviews %>%
        count(word, sort = TRUE) %>%
       filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      qt_reviews2 %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      qt_reviews3 %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)

