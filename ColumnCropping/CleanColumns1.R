library(magick)
library(dplyr)

len_of_list = lengths(x)
cleaned_column <- vector(mode = "list", length = len_of_list)

i <- 0
while (i < len_of_list){
    cleaned_column[[i]] <- str_replace_all(x[[1]][i], '[.]', ',')
    cleaned_column[[i]] <- str_replace_all(cleaned_column[[i]], '[.]', ',')
    i <- i + 1
}