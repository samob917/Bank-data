library(magick)
library(dplyr)

len_of_list = lengths(x)
cleaned_column <- vector(mode = "list", length = len_of_list) # this is a list

# we have a list of vectors to edit (splitColumns)

# this cleans one column - run for each numeric column (last 3, zipcodes should stay strings)
i <- 0
while (i < len_of_list){
    cleaned_column[[i]] <- str_replace_all(x[[1]][i], '[.]', ',')
    cleaned_column[[i]] <- str_replace_all(cleaned_column[[i]], '[,]', '')
    cleaned_column[[i]] <- str_replace_all(cleaned_column[[i]], '[o]', '0')
    cleaned_column[[i]] <- str_replace_all(cleaned_column[[i]], '[O]', '0')
    i <- i + 1
}

