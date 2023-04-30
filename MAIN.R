### Crystal Jones-Howe                 ###
### Olivia Mules                       ###
### INF 722:  Information Organization ###
### Professor Gangolly                 ###

###Use these libraries###
    library(tidyverse)
    library(tidytext)
    library(RColorBrewer)
    library(wordcloud2)
    library(htmlwidgets)
    library(webshot2)
    library(tm)


###Read in files###
  # set the directory containing the about files
      mtt <- read_file("data/MyTalkTools/MyTalkTools-About.txt")
      plq2g <- read_file("data/Proloquo2go/Proloquo2go-About.txt")
      qt <- read_file("data/QuickTalk/QuickTalk-About.txt")
      tc <- read_file("data/TouchChat/TouchChat-About.txt")

    
  # set the directory containing the review files
      file_dir_mtt <- "data/MyTalkTools/reviews/"
      file_dir_plq2g <- "data/Proloquo2go/reviews/"
      file_dir_qt <- "data/QuickTalk/reviews/"
      file_dir_tc <- "data/TouchChat/reviews/"
      file_dir_per <- "data/Personal/datatext"
    
  # get a list of file names in the directory
      file_paths_mtt <- list.files(file_dir_mtt, full.names = TRUE)
      file_paths_plq2g <- list.files(file_dir_plq2g, full.names = TRUE)
      file_paths_qt <- list.files(file_dir_qt, full.names = TRUE)
      file_paths_tc <- list.files(file_dir_tc, full.names = TRUE)
      file_paths_per <- list.files(file_dir_per, full.names = TRUE)

###Create files###
    # set the file path for the output file
      output_file_01 <- "data/MyTalkTools/mtt_about_01.csv"
      output_file_02 <- "data/MyTalkTools/mtt_about_02.csv"
      output_file_03 <- "data/MyTalkTools/mtt_about_03.csv"
      output_file_04 <- "data/MyTalkTools/mtt_reviews_01.csv"
      output_file_05 <- "data/MyTalkTools/mtt_reviews_02.csv"
      output_file_06 <- "data/MyTalkTools/mtt_reviews_03.csv"
      output_file_07 <- "data/Proloquo2go/plq2g_about_01.csv"
      output_file_08 <- "data/Proloquo2go/plq2g_about_02.csv"
      output_file_09 <- "data/Proloquo2go/plq2g_about_03.csv"
      output_file_10 <- "data/Proloquo2go/plq2g_reviews_01.csv"
      output_file_11 <- "data/Proloquo2go/plq2g_reviews_02.csv"
      output_file_12 <- "data/Proloquo2go/plq2g_reviews_03.csv"
      output_file_13 <- "data/QuickTalk/qt_about_01.csv"
      output_file_14 <- "data/QuickTalk/qt_about_02.csv"
      output_file_15 <- "data/QuickTalk/qt_about_03.csv"
      output_file_16 <- "data/QuickTalk/qt_reviews_01.csv"
      output_file_17 <- "data/QuickTalk/qt_reviews_02.csv"
      output_file_18 <- "data/QuickTalk/qt_reviews_03.csv"
      output_file_19 <- "data/TouchChat/tc_about_01.csv"
      output_file_20 <- "data/TouchChat/tc_about_02.csv"
      output_file_21 <- "data/TouchChat/tc_about_03.csv"
      output_file_22 <- "data/TouchChat/tc_reviews_01.csv"
      output_file_23 <- "data/TouchChat/tc_reviews_02.csv"
      output_file_24 <- "data/TouchChat/tc_reviews_03.csv"
      output_file_25 <- "data/Personal/per_processed_01.csv"
      output_file_26 <- "data/Personal/per_processed_02.csv"
      output_file_27 <- "data/Personal/per_processed_03.csv"
      #output_file__test <- "data/about_Test.csv"
      output_file_sw <- "data/our_stop_words.csv"
      output_file_tdm_unigrams <- "data/tdm_unigram.csv"
      output_file_tdm_bigrams <- "data/tdm_bigram.csv"
      output_file_tdm_trigrams <- "data/tdm_trigram.csv"
      
###Cleanup About Files###
  #Remove punctuation and replace with space
  #also removes extra white space
      mtt <- str_replace_all(mtt, "\\s+", " ") %>% 
        str_replace_all(., "\\p{Punct}", " ")  
      plq2g <- str_replace_all(plq2g, "\\s+", " ") %>% 
        str_replace_all(., "\\p{Punct}", " ")  
      qt <- str_replace_all(qt, "\\s+", " ") %>% 
        str_replace_all(., "\\p{Punct}", " ")  
      tc <- str_replace_all(tc, "\\s+", " ") %>% 
        str_replace_all(., "\\p{Punct}", " ")  

  #places about data into data frame table
      mtt_df_01 <- tibble(mtt)
      plq2g_df_01 <- tibble(plq2g)
      qt_df_01 <- tibble(qt)
      tc_df_01 <- tibble(tc)

  #loading dataset called stop_words
      data("stop_words")

  #include custom stop words
      custom_stop_words <- bind_rows(tibble(word = c("and","and and", "app","apps","apple","ipad","aac","touchchat","proloquo2go","ve",
                                                     "mytalktools","mytalk","don","too","iphone","day","pts","lot","lot",
                                                     "itunes","it's","i've","ipod","2","000","20","10","3","5"),
                                            lexicon = c("custom")),
                                     stop_words)


      
      
      
###Process Data###     
    #Create grams
          mtt_about_01 <- mtt_df_01 %>% unnest_tokens(word,mtt) 
          mtt_about_01 <- mtt_about_01 %>% anti_join(custom_stop_words)
          mtt_about_02 <- mtt_df_01 %>% unnest_tokens(word,mtt, token = "ngrams", n= 2) 
          mtt_about_02 <- mtt_about_02 %>% anti_join(custom_stop_words)
          mtt_about_03 <- mtt_df_01 %>% unnest_tokens(word,mtt, token = "ngrams", n= 3)
          mtt_about_03 <- mtt_about_03 %>% anti_join(custom_stop_words)
          plq2g_about_01 <- plq2g_df_01 %>% unnest_tokens(word,plq2g) 
          plq2g_about_01 <- plq2g_about_01 %>% anti_join(custom_stop_words)
          plq2g_about_02 <- plq2g_df_01 %>% unnest_tokens(word,plq2g, token = "ngrams", n= 2) 
          plq2g_about_02 <- plq2g_about_02 %>% anti_join(custom_stop_words)
          plq2g_about_03 <- plq2g_df_01 %>% unnest_tokens(word,plq2g, token = "ngrams", n= 3)
          plq2g_about_03 <- plq2g_about_03 %>% anti_join(custom_stop_words)
          qt_about_01 <- qt_df_01 %>% unnest_tokens(word,qt) 
          qt_about_01 <- qt_about_01 %>% anti_join(custom_stop_words)
          qt_about_02 <- qt_df_01 %>% unnest_tokens(word,qt, token = "ngrams", n= 2) 
          qt_about_02 <- qt_about_02 %>% anti_join(custom_stop_words)
          qt_about_03 <- qt_df_01 %>% unnest_tokens(word,qt, token = "ngrams", n= 3)
          qt_about_03 <- qt_about_03 %>% anti_join(custom_stop_words)
          tc_about_01 <- tc_df_01 %>% unnest_tokens(word,tc) 
          tc_about_01 <- tc_about_01 %>% anti_join(custom_stop_words)
          tc_about_02 <- tc_df_01 %>% unnest_tokens(word,tc, token = "ngrams", n= 2) 
          tc_about_02 <- tc_about_02 %>% anti_join(custom_stop_words)
          tc_about_03 <- tc_df_01 %>% unnest_tokens(word,tc, token = "ngrams", n= 3)
          tc_about_03 <- tc_about_03 %>% anti_join(custom_stop_words)

    #read in the review files and combine the data to make unigrams
          mtt_reviews_01 <- map(file_paths_mtt, read_file) %>% 
            reduce(c) %>% 
            tibble(text = .) %>% 
            unnest_tokens(word, text)
          plq2g_reviews_01 <- map(file_paths_plq2g, read_file) %>% 
            reduce(c) %>% 
            tibble(text = .) %>% 
            unnest_tokens(word, text)
          qt_reviews_01 <- map(file_paths_qt, read_file) %>% 
            reduce(c) %>% 
            tibble(text = .) %>% 
            unnest_tokens(word, text)
          tc_reviews_01 <- map(file_paths_tc, read_file) %>% 
            reduce(c) %>% 
            tibble(text = .) %>% 
            unnest_tokens(word, text)
          per_text_01 <- map(file_paths_per, read_file)  %>%
            reduce(c)  %>%
            tibble(text = .)  %>%
            unnest_tokens(word, text)
        
      #read in the files and combine the data to make bigrams
          mtt_reviews_02 <- map(file_paths_mtt, read_file) %>% 
           reduce(c) %>% 
           tibble(text = .) %>% 
           unnest_tokens(word, text, token = "ngrams", n= 2)
          plq2g_reviews_02 <- map(file_paths_plq2g, read_file) %>% 
            reduce(c) %>% 
            tibble(text = .) %>% 
            unnest_tokens(word, text, token = "ngrams", n= 2)
          qt_reviews_02 <- map(file_paths_qt, read_file) %>% 
            reduce(c) %>% 
            tibble(text = .) %>% 
            unnest_tokens(word, text, token = "ngrams", n= 2)
          tc_reviews_02 <- map(file_paths_tc, read_file) %>% 
            reduce(c) %>% 
            tibble(text = .) %>% 
            unnest_tokens(word, text, token = "ngrams", n= 2)
          per_text_02 <- map(file_paths_per, read_file)   %>% 
            reduce(c)  %>% 
            tibble(text = .)  %>% 
            unnest_tokens(word, text, token = "ngrams", n= 2)

      #read in the files and combine the data to make trigrams
          mtt_reviews_03 <- map(file_paths_mtt, read_file) %>% 
            reduce(c) %>% 
            tibble(text = .) %>% 
            unnest_tokens(word, text, token = "ngrams", n= 3)
          plq2g_reviews_03 <- map(file_paths_plq2g, read_file) %>% 
            reduce(c) %>% 
            tibble(text = .) %>% 
            unnest_tokens(word, text, token = "ngrams", n= 3)
          qt_reviews_03 <- map(file_paths_qt, read_file) %>% 
            reduce(c) %>% 
            tibble(text = .) %>% 
            unnest_tokens(word, text, token = "ngrams", n= 3)
          tc_reviews_03 <- map(file_paths_tc, read_file) %>% 
            reduce(c) %>% 
            tibble(text = .) %>% 
            unnest_tokens(word, text, token = "ngrams", n= 3)
          per_text_03 <- map(file_paths_per, read_file) %>% 
            reduce(c) %>% 
            tibble(text = .) %>% 
            unnest_tokens(word, text, token = "ngrams", n= 3)

      #removing all stop words
          mtt_about_01 <- mtt_about_01 %>% anti_join(custom_stop_words)
          mtt_reviews_01 <- mtt_reviews_01 %>% anti_join(custom_stop_words)
          mtt_reviews_02 <- mtt_reviews_02 %>% anti_join(custom_stop_words)
          mtt_reviews_03 <- mtt_reviews_03 %>% anti_join(custom_stop_words)
          plq2g_about_01 <- plq2g_about_01 %>% anti_join(custom_stop_words)
          plq2g_reviews_01 <- plq2g_reviews_01 %>% anti_join(custom_stop_words)
          plq2g_reviews_02 <- plq2g_reviews_02 %>% anti_join(custom_stop_words)
          plq2g_reviews_03 <- plq2g_reviews_03 %>% anti_join(custom_stop_words)
          qt_about_01 <- qt_about_01 %>% anti_join(custom_stop_words)
          qt_reviews_01 <- qt_reviews_01 %>% anti_join(custom_stop_words)
          qt_reviews_02 <- qt_reviews_02 %>% anti_join(custom_stop_words)
          qt_reviews_03 <- qt_reviews_03 %>% anti_join(custom_stop_words)
          tc_about_01 <- tc_about_01 %>% anti_join(custom_stop_words)
          tc_reviews_01 <- tc_reviews_01 %>% anti_join(custom_stop_words)
          tc_reviews_02 <- tc_reviews_02 %>% anti_join(custom_stop_words)
          tc_reviews_03 <- tc_reviews_03 %>% anti_join(custom_stop_words)
          per_text_01 <- per_text_01 %>% anti_join(custom_stop_words)
          per_text_02 <- per_text_02 %>% anti_join(custom_stop_words)
          per_text_03 <- per_text_03 %>% anti_join(custom_stop_words)
      
###Output Data###
    #term matrices
          my_corpus_01 <- data.frame()
          my_corpus_02 <- data.frame()
          my_corpus_03 <- data.frame()
          
      # Create a term document matrix of unigrams 
          my_corpus_01 <- VCorpus(VectorSource(c(mtt_about_01, plq2g_about_01, qt_about_01, tc_about_01, mtt_reviews_01, plq2g_reviews_01, qt_reviews_01, tc_reviews_01, per_text_01)))
          my_tokenizer_01 <- function(x) 
          {
            unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
          }
          tdm_01 <- TermDocumentMatrix(my_corpus_01, control = list(tokenize = my_tokenizer_01))
          tdm_matrix_01 <- as.matrix(tdm_01)
          #tdm_matrix_01
 
      # Create a term document matrix of bigrams
          my_corpus_02 <- VCorpus(VectorSource(c(mtt_about_01, plq2g_about_01, qt_about_01, tc_about_01, mtt_reviews_01, plq2g_reviews_01, qt_reviews_01, tc_reviews_01, per_text_01)))
          my_tokenizer_02 <- function(x) 
          {
            unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
            
          }
          tdm_02 <- TermDocumentMatrix(my_corpus_02, control = list(tokenize = my_tokenizer_02))
          tdm_matrix_02 <- as.matrix(tdm_02)
          tdm_matrix_02
  
      # Create a term document matrix of trigrams 
           my_corpus_03 <- VCorpus(VectorSource(c(mtt_about_01, plq2g_about_01, qt_about_01, tc_about_01, mtt_reviews_01, plq2g_reviews_01, qt_reviews_01, tc_reviews_01, per_text_01)))
           my_tokenizer_03 <- function(x) 
           {
             unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
           }
           tdm_03 <- TermDocumentMatrix(my_corpus_03, control = list(tokenize = my_tokenizer_03))
           tdm_matrix_03 <- as.matrix(tdm_03)
           #tdm_matrix_03

    #word count
          mtt_about_01 <- mtt_about_01 %>% count(word, sort = TRUE)
          mtt_about_02 <-mtt_about_02 %>% count(word, sort = TRUE)
          mtt_about_03 <-mtt_about_03 %>% count(word, sort = TRUE)
          mtt_reviews_01 <- mtt_reviews_01 %>% count(word, sort = TRUE)
          mtt_reviews_02 <- mtt_reviews_02 %>% count(word, sort = TRUE)
          mtt_reviews_03 <- mtt_reviews_03 %>% count(word, sort = TRUE)
          plq2g_about_01 <- plq2g_about_01 %>% count(word, sort = TRUE)
          plq2g_about_02 <-plq2g_about_02 %>% count(word, sort = TRUE)
          plq2g_about_03 <-plq2g_about_03 %>% count(word, sort = TRUE)
          plq2g_reviews_01 <- plq2g_reviews_01 %>% count(word, sort = TRUE)
          plq2g_reviews_02 <- plq2g_reviews_02 %>% count(word, sort = TRUE)
          plq2g_reviews_03 <- plq2g_reviews_03 %>% count(word, sort = TRUE)
          qt_about_01 <- qt_about_01 %>% count(word, sort = TRUE)
          qt_about_02 <-qt_about_02 %>% count(word, sort = TRUE)
          qt_about_03 <-qt_about_03 %>% count(word, sort = TRUE)
          qt_reviews_01 <- qt_reviews_01 %>% count(word, sort = TRUE)
          qt_reviews_02 <- qt_reviews_02 %>% count(word, sort = TRUE)
          qt_reviews_03 <- qt_reviews_03 %>% count(word, sort = TRUE)
          tc_about_01 <- tc_about_01 %>% count(word, sort = TRUE)
          tc_about_02 <-tc_about_02 %>% count(word, sort = TRUE)
          tc_about_03 <-tc_about_03 %>% count(word, sort = TRUE)
          tc_reviews_01 <- tc_reviews_01 %>% count(word, sort = TRUE)
          tc_reviews_02 <- tc_reviews_02 %>% count(word, sort = TRUE)
          tc_reviews_03 <- tc_reviews_03 %>% count(word, sort = TRUE)
          per_text_01 <- per_text_01 %>% count(word, sort = TRUE)
          per_text_02 <- per_text_02 %>% count(word, sort = TRUE)
          per_text_03 <- per_text_03 %>% count(word, sort = TRUE)

     # write the combined data frame to a CSV file
          write_csv(mtt_about_01, output_file_01)
          write_csv(mtt_about_02, output_file_02)
          write_csv(mtt_about_03, output_file_03)
          write_csv(mtt_reviews_01, output_file_04)
          write_csv(mtt_reviews_02, output_file_05)
          write_csv(mtt_reviews_03, output_file_06)
          write_csv(plq2g_about_01, output_file_07)
          write_csv(plq2g_about_02, output_file_08)
          write_csv(plq2g_about_03, output_file_09)
          write_csv(plq2g_reviews_01, output_file_10)
          write_csv(plq2g_reviews_02, output_file_11)
          write_csv(plq2g_reviews_03, output_file_12)
          write_csv(qt_about_01, output_file_13)
          write_csv(qt_about_02, output_file_14)
          write_csv(qt_about_03, output_file_15)
          write_csv(qt_reviews_01, output_file_16)
          write_csv(qt_reviews_02, output_file_17)
          write_csv(qt_reviews_03, output_file_18)
          write_csv(tc_about_01, output_file_19)
          write_csv(tc_about_02, output_file_20)
          write_csv(tc_about_03, output_file_21)
          write_csv(tc_reviews_01, output_file_22)
          write_csv(tc_reviews_02, output_file_23)
          write_csv(tc_reviews_03, output_file_24)
          write_csv(per_text_01, output_file_25)
          write_csv(per_text_02, output_file_26)
          write_csv(per_text_03, output_file_27)
          write_csv(custom_stop_words, output_file_sw)
          write.csv(as.matrix(tdm_matrix_01), file = output_file_tdm_unigrams)
          write.csv(as.matrix(tdm_matrix_02), file = output_file_tdm_bigrams)
          write.csv(as.matrix(tdm_matrix_03), file = output_file_tdm_trigrams)

#     # #frequency matrices
#     #       freq_matrix <- table(mtt_about_01$word, mtt_about_01$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(mtt_about_012$word, mtt_about_012$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(mtt_about_03$word, mtt_about_03$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(mtt_reviews01$word, mtt_reviews01$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(mtt_reviews2$word, mtt_reviews2$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(mtt_reviews3$word, mtt_reviews3$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(plq2g_about$word, plq2g_about$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(plq2g_about2$word, plq2g_about2$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(plq2g_about3$word, plq2g_about3$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(plq2g_reviews$word, plq2g_reviews$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(plq2g_reviews2$word, plq2g_reviews2$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(plq2g_reviews3$word, plq2g_reviews3$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(qt_about$word, qt_about$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(qt_about2$word, qt_about2$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(qt_about3$word, qt_about3$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(qt_reviews$word, qt_reviews$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(qt_reviews2$word, qt_reviews2$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(qt_reviews3$word, qt_reviews3$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(tc_about$word, tc_about$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(tc_about2$word, tc_about2$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(tc_about3$word, tc_about3$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(tc_reviews$word, tc_reviews$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(tc_reviews2$word, tc_reviews2$n)
#     #       print(freq_matrix)
#     #       freq_matrix <- table(tc_reviews3$word, tc_reviews3$n)
#     #       print(freq_matrix)
# 
# 
# ###Visualizations###
#     # #frequency graph
#     #   mtt_about_01 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   mtt_about_02 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   mtt_about_03 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   mtt_reviews_01 %>%
#     #     count(word, sort = TRUE) %>%
#     #    filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   mtt_reviews_02 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   mtt_reviews_03 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   plq2g_about_01 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   plq2g_about_02 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   plq2g_about_03 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   plq2g_reviews_01 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   plq2g_reviews_02 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   plq2g_reviews_03 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   qt_about_01 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   qt_about_02 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   qt_about_03 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   qt_reviews_04 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   qt_reviews_02 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   qt_reviews_03 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   tc_about_01 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   tc_about_02 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   tc_about_03 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   tc_reviews_01 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   tc_reviews_02 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#     #   tc_reviews_03 %>%
#     #     count(word, sort = TRUE) %>%
#     #     filter(n > 0) %>%
#     #     mutate(word = reorder(word, n)) %>%
#     #     ggplot(aes(n, word)) +
#     #     geom_col() +
#     #     labs(y = NULL)
#       
# 
#       
#   #Frequency calc      
#       frequency <- bind_rows(mutate(tc_about_01, product = "MyTalkTools"),
#                              mutate(qt_about_01, product = "QuickTalk"), 
#                              mutate(mtt_about_01, product = "TouchChat"),
#                              mutate(plq2g_about_01, product = "Proloquo2go")) %>% 
#         mutate(word = str_extract(word, "[a-z']+")) %>%
#         count(product, word) %>%
#         group_by(product) %>%
#         mutate(proportion = n / sum(n)) %>% 
#         select(-n) %>% 
#         pivot_wider(names_from = product, values_from = proportion) %>%
#         pivot_longer(`TouchChat`:`QuickTalk`:`MyTalkTools`:`Proloquo2go`,
#                      names_to = "product", values_to = "proportion")
#       
#   # #Scatterplots      
#   #     # expect a warning about rows with missing values being removed
#   #     ggplot(frequency, aes(x = proportion, y = `MyTalkTools`, 
#   #                           color = abs(`MyTalkTools` - proportion))) +
#   #       geom_abline(color = "gray40", lty = 2) +
#   #       geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
#   #       geom_text(aes(label = paste(word, collapse = ", ")), check_overlap = TRUE, vjust = 1.5) +
#   #       scale_x_log10(labels = percent_format()) +
#   #       scale_y_log10(labels = percent_format()) +
#   #       scale_color_gradient(limits = c(0, 0.001), 
#   #                            low = "darkslategray4", high = "gray75") +
#   #       facet_wrap(~product, ncol = 2) +
#   #       theme(legend.position="none") +
#   #       labs(y = "MyTalkTools", x = NULL)
# 
#WordClouds
        set.seed(1234) # for reproducibility


        my_graph <- wordcloud2(mtt_about_01, size=1.0)
        saveWidget(my_graph,"data/tmp.html",selfcontained = F)
        webshot("data/tmp.html","data/WordClouds/mtt_about_01.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp.html")
        unlink("data/tmp_files", recursive = TRUE)

        my_graph <- wordcloud2(mtt_about_02, size=1.0)
        saveWidget(my_graph,"data/tmp2.html",selfcontained = F)
        webshot("data/tmp2.html","data/WordClouds/mtt_about_02.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp2.html")
        unlink("data/tmp2_files", recursive = TRUE)

        my_graph <- wordcloud2(mtt_about_03, size=0.5)
        saveWidget(my_graph,"data/tmp3.html",selfcontained = F)
        webshot("data/tmp3.html","data/WordClouds/mtt_about_03.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp3.html")
        unlink("data/tmp3_files", recursive = TRUE)

        my_graph <- wordcloud2(mtt_reviews_01, size=3.0)
        saveWidget(my_graph,"data/tmp4.html",selfcontained = F)
        webshot("data/tmp4.html","data/WordClouds/mtt_reviews_01.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp4.html")
        unlink("data/tmp4_files", recursive = TRUE)

        my_graph <- wordcloud2(mtt_reviews_02, size=3.0)
        saveWidget(my_graph,"data/tmp5.html",selfcontained = F)
        webshot("data/tmp5.html","data/WordClouds/mtt_reviews_02.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp5.html")
        unlink("data/tmp5_files", recursive = TRUE)

        my_graph <- wordcloud2(mtt_reviews_03, size=0.75)
        saveWidget(my_graph,"data/tmp6.html",selfcontained = F)
        webshot("data/tmp6.html","data/WordClouds/mtt_reviews_03.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp6.html")
        unlink("data/tmp6_files", recursive = TRUE)

        my_graph <- wordcloud2(plq2g_about_01, size=3.0)
        saveWidget(my_graph,"data/tmp7.html",selfcontained = F)
        webshot("data/tmp7.html","data/WordClouds/plq2g_about_01.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp7.html")
        unlink("data/tmp7_files", recursive = TRUE)

        my_graph <- wordcloud2(plq2g_about_02, size=3.0)
        saveWidget(my_graph,"data/tmp8.html",selfcontained = F)
        webshot("data/tmp8.html","data/WordClouds/plq2g_about_02.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp8.html")
        unlink("data/tmp8_files", recursive = TRUE)

        my_graph <- wordcloud2(plq2g_about_03, size=1.0)
        saveWidget(my_graph,"data/tmp9.html",selfcontained = F)
        webshot("data/tmp9.html","data/WordClouds/plq2g_about_03.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp9.html")
        unlink("data/tmp9_files", recursive = TRUE)

        my_graph <- wordcloud2(plq2g_reviews_01, size=3.0)
        saveWidget(my_graph,"data/tmp10.html",selfcontained = F)
        webshot("data/tmp10.html","data/WordClouds/plq2g_reviews_01.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp10.html")
        unlink("data/tmp10_files", recursive = TRUE)

        my_graph <- wordcloud2(plq2g_reviews_02, size=3.0)
        saveWidget(my_graph,"data/tmp11.html",selfcontained = F)
        webshot("data/tmp11.html","data/WordClouds/plq2g_reviews_02.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp11.html")
        unlink("data/tmp11_files", recursive = TRUE)

        my_graph <- wordcloud2(plq2g_reviews_03, size=1.0)
        saveWidget(my_graph,"data/tmp12.html",selfcontained = F)
        webshot("data/tmp12.html","data/WordClouds/plq2g_reviews_03.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp12.html")
        unlink("data/tmp12_files", recursive = TRUE)

        my_graph <- wordcloud2(qt_about_01, size=3.0)
        saveWidget(my_graph,"data/tmp13.html",selfcontained = F)
        webshot("data/tmp13.html","data/WordClouds/qt_about_01.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp13.html")
        unlink("data/tmp13_files", recursive = TRUE)

        my_graph <- wordcloud2(qt_about_02, size=3.0)
        saveWidget(my_graph,"data/tmp14.html",selfcontained = F)
        webshot("data/tmp14.html","data/WordClouds/qt_about_02.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp14.html")
        unlink("data/tmp14_files", recursive = TRUE)

        my_graph <- wordcloud2(qt_about_03, size=1.0)
        saveWidget(my_graph,"data/tmp15.html",selfcontained = F)
        webshot("data/tmp15.html","data/WordClouds/qt_about_03.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp15.html")
        unlink("data/tmp15_files", recursive = TRUE)

        my_graph <- wordcloud2(qt_reviews_01, size=3.0)
        saveWidget(my_graph,"data/tmp16.html",selfcontained = F)
        webshot("data/tmp16.html","data/WordClouds/qt_reviews_01.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp16.html")
        unlink("data/tmp16_files", recursive = TRUE)

        my_graph <- wordcloud2(qt_reviews_02, size=1.0)
        saveWidget(my_graph,"data/tmp17.html",selfcontained = F)
        webshot("data/tmp17.html","data/WordClouds/qt_reviews_02.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp17.html")
        unlink("data/tmp17_files", recursive = TRUE)

        my_graph <- wordcloud2(qt_reviews_03, size=1.0)
        saveWidget(my_graph,"data/tmp18.html",selfcontained = F)
        webshot("data/tmp18.html","data/WordClouds/qt_reviews_03.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp18.html")
        unlink("data/tmp18_files", recursive = TRUE)

        my_graph <- wordcloud2(tc_about_01, size=3.0)
        saveWidget(my_graph,"data/tmp19.html",selfcontained = F)
        webshot("data/tmp19.html","data/WordClouds/tc_about_01.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp19.html")
        unlink("data/tmp19_files", recursive = TRUE)

        my_graph <- wordcloud2(tc_about_02, size=3.0)
        saveWidget(my_graph,"data/tmp20.html",selfcontained = F)
        webshot("data/tmp20.html","data/WordClouds/tc_about_02.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp20.html")
        unlink("data/tmp20_files", recursive = TRUE)

        my_graph <- wordcloud2(tc_about_03, size=1.0)
        saveWidget(my_graph,"data/tmp21.html",selfcontained = F)
        webshot("data/tmp21.html","data/WordClouds/tc_about_03.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp21.html")
        unlink("data/tmp21_files", recursive = TRUE)

        my_graph <- wordcloud2(tc_reviews_01, size=2.0)
        saveWidget(my_graph,"data/tmp22.html",selfcontained = F)
        webshot("data/tmp22.html","data/WordClouds/tc_reviews_01.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp22.html")
        unlink("data/tmp22_files", recursive = TRUE)

        my_graph <- wordcloud2(tc_reviews_02, size=3.0)
        saveWidget(my_graph,"data/tmp23.html",selfcontained = F)
        webshot("data/tmp23.html","data/WordClouds/tc_reviews_02.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp23.html")
        unlink("data/tmp23_files", recursive = TRUE)

        my_graph <- wordcloud2(tc_reviews_03, size=1.0)
        saveWidget(my_graph,"data/tmp24.html",selfcontained = F)
        webshot("data/tmp24.html","data/WordClouds/tc_reviews_03.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp24.html")
        unlink("data/tmp24_files", recursive = TRUE)

        my_graph <- wordcloud2(per_text_01, size=3.0)
        saveWidget(my_graph,"data/tmp25.html",selfcontained = F)
        webshot("data/tmp25.html","data/WordClouds/per_text_01.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp25.html")
        unlink("data/tmp25_files", recursive = TRUE)

        my_graph <- wordcloud2(per_text_02, size=3.0)
        saveWidget(my_graph,"data/tmp26.html",selfcontained = F)
        webshot("data/tmp26.html","data/WordClouds/per_text_02.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp26.html")
        unlink("data/tmp26_files", recursive = TRUE)

        my_graph <- wordcloud2(per_text_03, size=1.0)
        saveWidget(my_graph,"data/tmp27.html",selfcontained = F)
        webshot("data/tmp27.html","data/WordClouds/per_text_03.png", delay = 5, vwidth = 2000, vheight = 2000)
        file.remove("data/tmp27.html")
        unlink("data/tmp27_files", recursive = TRUE)





