library(magick)
library(stringr)


# x is a list of vectors?
col1 <- c("he.llo", "my", "o", "na,me", "O", "is", "Ca,,.sey")
col2 <- c("Heldl.o,", "O.,", "Casey", "nice", "to", "mee,t", "O")
x <- data.frame("col1" = col1, "col2" = col2)
print(x)

# we have a list of vectors to edit (splitColumns)

# this cleans one column - run for each numeric column (last 3, zipcodes should stay strings)

    
    
    
while (i < len_of_list) {
    cleaned_column[[i]] <- str_replace_all(x[[1]][i], '[.]', ',')
    cleaned_column[[i]] <- str_replace_all(cleaned_column[[i]], '[,]', '')
    cleaned_column[[i]] <- str_replace_all(cleaned_column[[i]], '[o]', '0')
    cleaned_column[[i]] <- str_replace_all(cleaned_column[[i]], '[O]', '0')
    i <- i + 1
}
print(cleaned_column)

