library(tidyverse)
library(tidytext)

###Read in files###
    plq2g <- read_file("data/Proloquo2go/Proloquo2go-About.txt")
  # set the directory containing the review files
    file_dir <- "data/Proloquo2go/reviews/"
  # get a list of file names in the directory
    file_paths <- list.files(file_dir, full.names = TRUE)
    
###Create files###
    # set the file path for the output file
      output_file <- "data/Proloquo2go/plq2g_about.csv"
      output_file2 <- "data/Proloquo2go/plq2g_about_2.csv"
      output_file3 <- "data/Proloquo2go/plq2g_about_3.csv"
      output_file4 <- "data/Proloquo2go/plq2g_reviews.csv"
      output_file5 <- "data/Proloquo2go/plq2g_reviews2.csv"
      output_file6 <- "data/Proloquo2go/plq2g_reviews3.csv"
      
###Cleanup About Files###
  #Remove punctuation and replace with space
  #also removes extra white space
      plq2g <- str_replace_all(plq2g, "\\s+", " ") %>% 
        str_replace_all(., "\\p{Punct}", " ")  
  #places about data into data frame table
      plq2g_df <- tibble(plq2g)
  #loading dataset called stop_words
      data("stop_words")
  #include custom stop words
      custom_stop_words <- bind_rows(tibble(word = c("app","apps","apple","ipad","aac","touchchat","Proloquo2go","ve",
                                                     "Proloquo2go","mytalk","don","too","iphone","day","pts","lot","lot",
                                                     "itunes","it's","i've","ipod","2","000","20","10","3","5"),
                                            lexicon = c("custom")),
                                     stop_words)
###Process Data###      
      plq2g_about <- plq2g_df %>% unnest_tokens(word,plq2g) 
      plq2g_about <- plq2g_about %>% anti_join(custom_stop_words)
      plq2g_about2 <- plq2g_df %>% unnest_tokens(word,plq2g, token = "ngrams", n= 2) 
      plq2g_about2 <- plq2g_about2 %>% anti_join(custom_stop_words)
      plq2g_about3 <- plq2g_df %>% unnest_tokens(word,plq2g, token = "ngrams", n= 3)
      plq2g_about3 <- plq2g_about3 %>% anti_join(custom_stop_words)

  # read in the files and combine the data
    plq2g_reviews <- map(file_paths, read_file) %>% 
    # combine the text into a single character vector
      reduce(c) %>% 
    # convert to a data frame
      tibble(text = .) %>% 
    # separate the text into words
      unnest_tokens(word, text)
    # read in the files and combine the data
      plq2g_reviews2 <- map(file_paths, read_file) %>% 
    # combine the text into a single character vector
      reduce(c) %>% 
    # convert to a data frame
      tibble(text = .) %>% 
    # separate the text into words
      unnest_tokens(word, text, token = "ngrams", n= 2)

      plq2g_reviews3 <- map(file_paths, read_file) %>% 
    # combine the text into a single character vector
      reduce(c) %>% 
    # convert to a data frame
      tibble(text = .) %>% 
    # separate the text into words
      unnest_tokens(word, text, token = "ngrams", n= 3)
  
    #removing all stop words
      plq2g_reviews <- plq2g_reviews %>% anti_join(custom_stop_words)
      plq2g_reviews2 <- plq2g_reviews2 %>% anti_join(custom_stop_words)
      plq2g_reviews3 <- plq2g_reviews3 %>% anti_join(custom_stop_words)
      



###Output Data###
    #word count
      plq2g_about <- plq2g_about %>% count(word, sort = TRUE) 
      plq2g_about2 <-plq2g_about2 %>% count(word, sort = TRUE) 
      plq2g_about3 <-plq2g_about3 %>% count(word, sort = TRUE) 
      plq2g_reviews <- plq2g_reviews %>% count(word, sort = TRUE) 
      plq2g_reviews2 <- plq2g_reviews2 %>% count(word, sort = TRUE) 
      plq2g_reviews3 <- plq2g_reviews3 %>% count(word, sort = TRUE) 

   # write the combined data frame to a CSV file
      write_csv(plq2g_about, output_file)
      write_csv(plq2g_about2, output_file2)
      write_csv(plq2g_about3, output_file3)
      write_csv(plq2g_reviews, output_file4)
      write_csv(plq2g_reviews2, output_file5)
      write_csv(plq2g_reviews3, output_file6)

   # frequency matrices
      freq_matrix <- table(plq2g_about$word, plq2g_about$n)
      print(freq_matrix)
      freq_matrix <- table(plq2g_about2$word, plq2g_about2$n)
      print(freq_matrix)
      freq_matrix <- table(plq2g_about3$word, plq2g_about3$n)
      print(freq_matrix)
      freq_matrix <- table(plq2g_reviews$word, plq2g_reviews$n)
      print(freq_matrix)
      freq_matrix <- table(plq2g_reviews2$word, plq2g_reviews2$n)
      print(freq_matrix)
      freq_matrix <- table(plq2g_reviews3$word, plq2g_reviews3$n)
      print(freq_matrix)
      


###Create Graphs###
    #frequency graph
      plq2g_about %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      plq2g_about2 %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      plq2g_about3 %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      plq2g_reviews %>%
        count(word, sort = TRUE) %>%
       filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      plq2g_reviews2 %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)
      plq2g_reviews3 %>%
        count(word, sort = TRUE) %>%
        filter(n > 0) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)

