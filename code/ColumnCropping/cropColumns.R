#install.packages("tesseract")
#install.packages("magick")
library(tesseract)
library(magick)

########################################################################################
#  Given the name of a one page pdf, crops each column using predetermined pixel counts
#  Returns a vector of all data in columns. 
########################################################################################


#################### Using smart border-cropping ##############################

source("code/ColumnCropping/cropPage.R")

cropColumns <- function(image, pageNum) {

    # stores percentage that each column should take up
    col_pct <- c(0.36643, 0.15765, 0.09054, 0.12356, 0.09906, 0.04261)
    
    columns <- vector("character", 6) # will hold OCR-extracted text for each column
        
    # INITIAL CROP USING cropPage.R
        cropPageReturns <- cropPage(image, pageNum)
        cropped <- cropPageReturns[[1]]
        # #if (!file.exists(paste0("test/Page", page))) { # create a folder to hold crops for this page
        #     dir.create(paste0("test/Page", page))
        # }
        # image_write(cropped, path = paste0("test/Page", page, "/cropped.pdf"), format = "pdf") # saved cropped page to folder
        width <- as.numeric(image_info(cropped)[2]) # reset width and height for cropped page
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
            # image_write(column, path = paste0("test/Page", page, "/col", j, ".pdf"), format = "pdf")
        }
        
    r <- list(columns, cropPageReturns[2])    
        
        
    return(r)
}
