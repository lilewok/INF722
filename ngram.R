library(ngram)
library(text2vec)

file_path <- "C:\\Users/Crystal/OneDrive - University at Albany - SUNY/INF 722/Project/text/MyTalkTools-About.txt"

text_lines <- readLines(file_path)

# Concatenate the lines into a single string
text <- paste(text_lines, collapse = " ")

preprocess(text, remove.punct=TRUE, remove.numbers=TRUE)

options(max.print = .Machine$integer.max)

ngram_obj_3 <- ngram(text, 3)
print(ngram_obj_3, output = "full")

#ngram_obj_2 <- ngram(text, 2)
#print(ngram_obj_2, output = "full")

#ngram_obj_1 <- ngram(text, n=1)
#print(ngram_obj_1, output = "full")

# Open a file for writing
fileConn <- file("my_file.txt", "w")

# Convert the ngrams to a single string
ngrams_string <- paste(unlist(ngram_obj_3), collapse = " ")

# Write text to the file
write(ngrams_string, fileConn)

# Close the file connection
close(fileConn)
