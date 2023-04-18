# Define a function to extract bigrams and trigrams from a text
extract_ngrams <- function(text) 
{
  library(ngram)
  ngram(, n=2)
  
#  words <- tolower(strsplit(text, "\\s+")[[1]])
#  bigrams <- paste(words[-length(words)], words[-1])
#  trigrams <- paste(words[1:(length(words)-2)], words[2:(length(words)-1)], words[3:length(words)])
#  return(c(bigrams, trigrams))
}

# Set the directory containing the text files
dir <- "C:\\Users/Crystal/OneDrive - University at Albany - SUNY/INF 722/Project/text"

# Get the list of text files in the directory
files <- list.files(dir, pattern = "\\.txt$", full.names = TRUE)

# Extract bigrams and trigrams from each text file
ngrams_list <- lapply(files, function(file) 
{
  text <- readLines(file, encoding = "UTF-8")
  ngrams <- extract_ngrams(text)
  return(ngrams)
})

options(max.print = .Machine$integer.max)

# Convert the list of ngrams to a vector
ngrams <- unlist(ngrams_list)
#print (ngrams)

# Create a frequency table of ngrams
freq_table <- table(ngrams)

# Convert the frequency table to a matrix
freq_matrix <- as.matrix(freq_table)

print(freq_matrix)
