########################################################################################
#  Identifies county and index if exists, output index of -1 otherwise
########################################################################################


findCounty <- function(columnOne) {
    county <- ""
    index <- -1
    
    # extract and identify county, delete those rows from col1 - assumes there is a county header
    indexOfCounty <- grep("(", columnOne, fixed = TRUE)
    
    if (len(indexOfCounty) != 0) {
        county <- strsplit(columnOne[indexOfParen], " ")[[1]][1]
        index <- indexOfCounty
    }
    
    r <- list(index, county)
    
    return(r)
}