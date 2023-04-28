###Use these libraries###
  library(tidyverse)
  library(tidytext)
  library(wordcloud)
  library(RColorBrewer)
  library(wordcloud2)
  library(scales)
  library(webshot)
  
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
    
  # get a list of file names in the directory
    file_paths_mtt <- list.files(file_dir_mtt, full.names = TRUE)
    file_paths_plq2g <- list.files(file_dir_plq2g, full.names = TRUE)
    file_paths_qt <- list.files(file_dir_qt, full.names = TRUE)
    file_paths_tc <- list.files(file_dir_tc, full.names = TRUE)

###Create files###
    # set the file path for the output file
      output_file01 <- "data/MyTalkTools/mtt_about.csv"
      output_file02 <- "data/MyTalkTools/mtt_about_2.csv"
      output_file03 <- "data/MyTalkTools/mtt_about_3.csv"
      output_file04 <- "data/MyTalkTools/mtt_reviews.csv"
      output_file05 <- "data/MyTalkTools/mtt_reviews2.csv"
      output_file06 <- "data/MyTalkTools/mtt_reviews3.csv"
      output_file07 <- "data/Proloquo2go/plq2g_about.csv"
      output_file08 <- "data/Proloquo2go/plq2g_about_2.csv"
      output_file09 <- "data/Proloquo2go/plq2g_about_3.csv"
      output_file10 <- "data/Proloquo2go/plq2g_reviews.csv"
      output_file11 <- "data/Proloquo2go/plq2g_reviews2.csv"
      output_file12 <- "data/Proloquo2go/plq2g_reviews3.csv"
      output_file13 <- "data/QuickTalk/qt_about.csv"
      output_file14 <- "data/QuickTalk/qt_about_2.csv"
      output_file15 <- "data/QuickTalk/qt_about_3.csv"
      output_file16 <- "data/QuickTalk/qt_reviews.csv"
      output_file17 <- "data/QuickTalk/qt_reviews2.csv"
      output_file18 <- "data/QuickTalk/qt_reviews3.csv"
      output_file19 <- "data/TouchChat/tc_about.csv"
      output_file20 <- "data/TouchChat/tc_about_2.csv"
      output_file21 <- "data/TouchChat/tc_about_3.csv"
      output_file22 <- "data/TouchChat/tc_reviews.csv"
      output_file23 <- "data/TouchChat/tc_reviews2.csv"
      output_file24 <- "data/TouchChat/tc_reviews3.csv"
      output_file_test <- "data/about_Test.csv"
      output_file_sw <- "data/our_stop_words.csv"
      
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
      mtt_df <- tibble(mtt)
      plq2g_df <- tibble(plq2g)
      qt_df <- tibble(qt)
      tc_df <- tibble(tc)

  #loading dataset called stop_words
      data("stop_words")

  #include custom stop words
      custom_stop_words <- bind_rows(tibble(word = c("app","apps","apple","ipad","aac","touchchat","proloquo2go","ve",
                                                     "mytalktools","mytalk","don","too","iphone","day","pts","lot","lot",
                                                     "itunes","it's","i've","ipod","2","000","20","10","3","5"),
                                            lexicon = c("custom")),
                                     stop_words)
      write_csv(custom_stop_words, output_file_sw)
###Process Data###     
  #Create grams
      mtt_about <- mtt_df %>% unnest_tokens(word,mtt) 
      mtt_about <- mtt_about %>% anti_join(custom_stop_words)
      mtt_about2 <- mtt_df %>% unnest_tokens(word,mtt, token = "ngrams", n= 2) 
      mtt_about2 <- mtt_about2 %>% anti_join(custom_stop_words)
      mtt_about3 <- mtt_df %>% unnest_tokens(word,mtt, token = "ngrams", n= 3)
      mtt_about3 <- mtt_about3 %>% anti_join(custom_stop_words)
      plq2g_about <- plq2g_df %>% unnest_tokens(word,plq2g) 
      plq2g_about <- plq2g_about %>% anti_join(custom_stop_words)
      plq2g_about2 <- plq2g_df %>% unnest_tokens(word,plq2g, token = "ngrams", n= 2) 
      plq2g_about2 <- plq2g_about2 %>% anti_join(custom_stop_words)
      plq2g_about3 <- plq2g_df %>% unnest_tokens(word,plq2g, token = "ngrams", n= 3)
      plq2g_about3 <- plq2g_about3 %>% anti_join(custom_stop_words)
      qt_about <- qt_df %>% unnest_tokens(word,qt) 
      qt_about <- qt_about %>% anti_join(custom_stop_words)
      qt_about2 <- qt_df %>% unnest_tokens(word,qt, token = "ngrams", n= 2) 
      qt_about2 <- qt_about2 %>% anti_join(custom_stop_words)
      qt_about3 <- qt_df %>% unnest_tokens(word,qt, token = "ngrams", n= 3)
      qt_about3 <- qt_about3 %>% anti_join(custom_stop_words)
      tc_about <- tc_df %>% unnest_tokens(word,tc) 
      tc_about <- tc_about %>% anti_join(custom_stop_words)
      tc_about2 <- tc_df %>% unnest_tokens(word,tc, token = "ngrams", n= 2) 
      tc_about2 <- tc_about2 %>% anti_join(custom_stop_words)
      tc_about3 <- tc_df %>% unnest_tokens(word,tc, token = "ngrams", n= 3)
      tc_about3 <- tc_about3 %>% anti_join(custom_stop_words)

    # read in the files and combine the data to make unigrams
      mtt_reviews <- map(file_paths_mtt, read_file) %>% 
        reduce(c) %>% 
        tibble(text = .) %>% 
        unnest_tokens(word, text)
      plq2g_reviews <- map(file_paths_plq2g, read_file) %>% 
        reduce(c) %>% 
        tibble(text = .) %>% 
        unnest_tokens(word, text)
      qt_reviews <- map(file_paths_qt, read_file) %>% 
        reduce(c) %>% 
        tibble(text = .) %>% 
        unnest_tokens(word, text)
      tc_reviews <- map(file_paths_tc, read_file) %>% 
        reduce(c) %>% 
        tibble(text = .) %>% 
        unnest_tokens(word, text)
        
    # read in the files and combine the data to make bigrams
      mtt_reviews2 <- map(file_paths_mtt, read_file) %>% 
       reduce(c) %>% 
       tibble(text = .) %>% 
       unnest_tokens(word, text, token = "ngrams", n= 2)
      plq2g_reviews2 <- map(file_paths_plq2g, read_file) %>% 
        reduce(c) %>% 
        tibble(text = .) %>% 
        unnest_tokens(word, text, token = "ngrams", n= 2)
      qt_reviews2 <- map(file_paths_qt, read_file) %>% 
        reduce(c) %>% 
        tibble(text = .) %>% 
        unnest_tokens(word, text, token = "ngrams", n= 2)
      tc_reviews2 <- map(file_paths_tc, read_file) %>% 
        reduce(c) %>% 
        tibble(text = .) %>% 
        unnest_tokens(word, text, token = "ngrams", n= 2)

   # read in the files and combine the data to make trigrams
      mtt_reviews3 <- map(file_paths_mtt, read_file) %>% 
        reduce(c) %>% 
        tibble(text = .) %>% 
        unnest_tokens(word, text, token = "ngrams", n= 3)
      plq2g_reviews3 <- map(file_paths_plq2g, read_file) %>% 
        reduce(c) %>% 
        tibble(text = .) %>% 
        unnest_tokens(word, text, token = "ngrams", n= 3)
      qt_reviews3 <- map(file_paths_qt, read_file) %>% 
        reduce(c) %>% 
        tibble(text = .) %>% 
        unnest_tokens(word, text, token = "ngrams", n= 3)
      tc_reviews3 <- map(file_paths_tc, read_file) %>% 
        reduce(c) %>% 
        tibble(text = .) %>% 
        unnest_tokens(word, text, token = "ngrams", n= 3)

    #removing all stop words
      mtt_reviews <- mtt_reviews %>% anti_join(custom_stop_words)
      mtt_reviews2 <- mtt_reviews2 %>% anti_join(custom_stop_words)
      mtt_reviews3 <- mtt_reviews3 %>% anti_join(custom_stop_words)
      plq2g_reviews <- plq2g_reviews %>% anti_join(custom_stop_words)
      plq2g_reviews2 <- plq2g_reviews2 %>% anti_join(custom_stop_words)
      plq2g_reviews3 <- plq2g_reviews3 %>% anti_join(custom_stop_words)
      qt_reviews <- qt_reviews %>% anti_join(custom_stop_words)
      qt_reviews2 <- qt_reviews2 %>% anti_join(custom_stop_words)
      qt_reviews3 <- qt_reviews3 %>% anti_join(custom_stop_words)
      tc_reviews <- tc_reviews %>% anti_join(custom_stop_words)
      tc_reviews2 <- tc_reviews2 %>% anti_join(custom_stop_words)
      tc_reviews3 <- tc_reviews3 %>% anti_join(custom_stop_words)
      

###Output Data###
    #word count
      mtt_about <- mtt_about %>% count(word, sort = TRUE) 
      mtt_about2 <-mtt_about2 %>% count(word, sort = TRUE) 
      mtt_about3 <-mtt_about3 %>% count(word, sort = TRUE) 
      mtt_reviews <- mtt_reviews %>% count(word, sort = TRUE) 
      mtt_reviews2 <- mtt_reviews2 %>% count(word, sort = TRUE) 
      mtt_reviews3 <- mtt_reviews3 %>% count(word, sort = TRUE) 
      plq2g_about <- plq2g_about %>% count(word, sort = TRUE) 
      plq2g_about2 <-plq2g_about2 %>% count(word, sort = TRUE) 
      plq2g_about3 <-plq2g_about3 %>% count(word, sort = TRUE) 
      plq2g_reviews <- plq2g_reviews %>% count(word, sort = TRUE) 
      plq2g_reviews2 <- plq2g_reviews2 %>% count(word, sort = TRUE) 
      plq2g_reviews3 <- plq2g_reviews3 %>% count(word, sort = TRUE) 
      qt_about <- qt_about %>% count(word, sort = TRUE) 
      qt_about2 <-qt_about2 %>% count(word, sort = TRUE) 
      qt_about3 <-qt_about3 %>% count(word, sort = TRUE) 
      qt_reviews <- qt_reviews %>% count(word, sort = TRUE) 
      qt_reviews2 <- qt_reviews2 %>% count(word, sort = TRUE) 
      qt_reviews3 <- qt_reviews3 %>% count(word, sort = TRUE) 
      tc_about <- tc_about %>% count(word, sort = TRUE) 
      tc_about2 <-tc_about2 %>% count(word, sort = TRUE) 
      tc_about3 <-tc_about3 %>% count(word, sort = TRUE) 
      tc_reviews <- tc_reviews %>% count(word, sort = TRUE) 
      tc_reviews2 <- tc_reviews2 %>% count(word, sort = TRUE) 
      tc_reviews3 <- tc_reviews3 %>% count(word, sort = TRUE) 
      
      

   # write the combined data frame to a CSV file
      write_csv(mtt_about, output_file01)
      write_csv(mtt_about2, output_file02)
      write_csv(mtt_about3, output_file03)
      write_csv(mtt_reviews, output_file04)
      write_csv(mtt_reviews2, output_file05)
      write_csv(mtt_reviews3, output_file06)
      write_csv(plq2g_about, output_file07)
      write_csv(plq2g_about2, output_file08)
      write_csv(plq2g_about3, output_file09)
      write_csv(plq2g_reviews, output_file10)
      write_csv(plq2g_reviews2, output_file11)
      write_csv(plq2g_reviews3, output_file12)
      write_csv(qt_about, output_file13)
      write_csv(qt_about2, output_file14)
      write_csv(qt_about3, output_file15)
      write_csv(qt_reviews, output_file16)
      write_csv(qt_reviews2, output_file17)
      write_csv(qt_reviews3, output_file18)
      write_csv(tc_about, output_file19)
      write_csv(tc_about2, output_file20)
      write_csv(tc_about3, output_file21)
      write_csv(tc_reviews, output_file22)
      write_csv(tc_reviews2, output_file23)
      write_csv(tc_reviews3, output_file24)

   # frequency matrices
      freq_matrix <- table(mtt_about$word, mtt_about$n)
      print(freq_matrix)
      freq_matrix <- table(mtt_about2$word, mtt_about2$n)
      print(freq_matrix)
      freq_matrix <- table(mtt_about3$word, mtt_about3$n)
      print(freq_matrix)
      freq_matrix <- table(mtt_reviews$word, mtt_reviews$n)
      print(freq_matrix)
      freq_matrix <- table(mtt_reviews2$word, mtt_reviews2$n)
      print(freq_matrix)
      freq_matrix <- table(mtt_reviews3$word, mtt_reviews3$n)
      print(freq_matrix)
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
    # #frequency graph
    #   mtt_about %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   mtt_about2 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   mtt_about3 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   mtt_reviews %>%
    #     count(word, sort = TRUE) %>%
    #    filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   mtt_reviews2 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   mtt_reviews3 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   plq2g_about %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   plq2g_about2 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   plq2g_about3 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   plq2g_reviews %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   plq2g_reviews2 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   plq2g_reviews3 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   qt_about %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   qt_about2 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   qt_about3 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   qt_reviews %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   qt_reviews2 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   qt_reviews3 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   tc_about %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   tc_about2 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   tc_about3 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   tc_reviews %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   tc_reviews2 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
    #   tc_reviews3 %>%
    #     count(word, sort = TRUE) %>%
    #     filter(n > 0) %>%
    #     mutate(word = reorder(word, n)) %>%
    #     ggplot(aes(n, word)) +
    #     geom_col() +
    #     labs(y = NULL)
      
#Test Area
      
      
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
      
  # #Scatterplots      
  #     # expect a warning about rows with missing values being removed
  #     ggplot(frequency, aes(x = proportion, y = `MyTalkTools`, 
  #                           color = abs(`MyTalkTools` - proportion))) +
  #       geom_abline(color = "gray40", lty = 2) +
  #       geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  #       geom_text(aes(label = paste(word, collapse = ", ")), check_overlap = TRUE, vjust = 1.5) +
  #       scale_x_log10(labels = percent_format()) +
  #       scale_y_log10(labels = percent_format()) +
  #       scale_color_gradient(limits = c(0, 0.001), 
  #                            low = "darkslategray4", high = "gray75") +
  #       facet_wrap(~product, ncol = 2) +
  #       theme(legend.position="none") +
  #       labs(y = "MyTalkTools", x = NULL)

  #WordClouds      
      set.seed(1234) # for reproducibility 

      wordcloud2(data=mtt_about, size=0.5, color='random-dark')
      wordcloud2(data=mtt_about2, size=0.5, color='random-dark')
      wordcloud2(data=mtt_about3, size=0.5, color='random-dark')
      wordcloud2(data=mtt_reviews, size=0.5, color='random-dark')
      wordcloud2(data=mtt_reviews2, size=0.5, color='random-dark')
      wordcloud2(data=mtt_reviews3, size=0.5, color='random-dark')
      wordcloud2(data=plq2g_about, size=0.5, color='random-dark')
      wordcloud2(data=plq2g_about2, size=0.5, color='random-dark')
      wordcloud2(data=plq2g_about3, size=0.5, color='random-dark')
      wordcloud2(data=plq2g_reviews, size=0.5, color='random-dark')
      wordcloud2(data=plq2g_reviews2, size=0.5, color='random-dark')
      wordcloud2(data=plq2g_reviews3, size=0.5, color='random-dark')
      wordcloud2(data=qt_about, size=0.5, color='random-dark')
      wordcloud2(data=qt_about2, size=0.5, color='random-dark')
      wordcloud2(data=qt_about3, size=0.5, color='random-dark')
      wordcloud2(data=qt_reviews, size=0.5, color='random-dark')
      wordcloud2(data=qt_reviews2, size=0.5, color='random-dark')
      wordcloud2(data=qt_reviews3, size=0.5, color='random-dark')
      wordcloud2(data=tc_about, size=0.5, color='random-dark')
      wordcloud2(data=tc_about2, size=0.5, color='random-dark')
      wordcloud2(data=tc_about3, size=0.5, color='random-dark')
      wordcloud2(data=tc_reviews, size=0.5, color='random-dark')
      wordcloud2(data=tc_reviews2, size=0.5, color='random-dark')
      wordcloud2(data=tc_reviews3, size=0.5, color='random-dark')
      
  # save the word cloud as a PNG file
      png(file = "my_wordcloud.png", width = 800, height = 600)
      mtt_about
      dev.off()
      
      