library(tidyverse)
library(tidytext)

# set the directory containing the files
file_dir <- "data/MyTalkTools/reviews/"

# get a list of file names in the directory
file_paths <- list.files(file_dir, full.names = TRUE)

# read in the files and combine the data
combined_data <- map(file_paths, read_file) %>% 
  # combine the text into a single character vector
  reduce(c) %>% 
  # convert to a data frame
  tibble(text = .) %>% 
  # separate the text into words
  unnest_tokens(word, text)

# set the file path for the output file
output_file <- "data/MyTalkTools/output.csv"

# write the combined data frame to a CSV file
write_csv(combined_data, output_file)

