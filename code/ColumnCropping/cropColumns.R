#install.packages("tesseract")
#install.packages("magick")
library(tesseract)
library(magick)

########################################################################################
#  Given the magick image file of 1 page and the page number, crops out column using 
#  predetermined column widths
#  Returns a list containing:
#    a vector containing the OCR-extracted text of each column
#    a string that is the state of the current page
########################################################################################


#################### Using smart border-cropping ##############################

source("code/ColumnCropping/cropPage.R")

cropColumns <- function(image, pageNum) {

    # stores percentage that each column should take up
    col_pct <- c(0.36643, 0.15765, 0.09054, 0.12356, 0.09906, 0.04261)
    
    columns <- vector("character", 6) # will hold OCR-extracted text for each column
        
    # INITIAL BORDER CROP using cropPage function
    cropPageReturns <- cropPage(image, pageNum)
    cropped <- cropPageReturns[[1]]
    width <- as.numeric(image_info(cropped)[2]) # set width and height for cropped page
    height <- as.numeric(image_info(cropped)[3])
    # CROP COLUMNS
    col_widths <- c(0, 0, 0, 0, 0, 0) # numeric vector to store pixel widths for each column
    for (j in 1:length(col_pct)) {
        col_widths[j] <- col_pct[j] * width
        # print(col_widths)
        if (j == length(col_pct)) {
            geo <- paste0(width - sum(col_widths[1:j-1]) - 100, 'x', height, '+', sum(col_widths[1:j]) - col_widths[j], '+', 0)
        } else {
            geo <- paste0(col_widths[j], 'x', height, '+', sum(col_widths[1:j]) - col_widths[j], '+', 0)
        }
        column <- image_crop(cropped, geo)
        columns[j] <- ocr(column, engine = tesseract("eng"))
    }
        
    r <- list(columns, cropPageReturns[[2]])    
        
        
    return(r)
}
