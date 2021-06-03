#install.packages("tesseract")
#install.packages("magick")
library(tesseract)
library(magick)

########################################################################################
#  Given the magick image file of 1 page and the page number, crops out column using 
#  predetermined column widths
#  Returns a list containing:
#    a string that is the state of the current page
#    vectors containing the OCR-extracted text of each column for each findAndCropped image
########################################################################################


#################### Using smart border-cropping ##############################

source("code/ColumnCropping/cropPage.R")
source("code/ColumnCropping/findCrop.R")

cropColumns <- function(image, pageNum) {

    # stores percentage that each column should take up
    #col_pct <- c(0.36643, 0.15765, 0.09054, 0.12356, 0.09906, 0.04261)
    col_pct <- c(0.3863, 0.1608, 0.0906, 0.1202, 0.1127, 0.1158)
        
    # INITIAL BORDER CROP using cropPage function
    cropPageReturns <- cropPage(image, pageNum)
    if (is.null(cropPageReturns[[1]]) & is.null(cropPageReturns[[2]])) {
        return(cropPageReturns) # return null list to indicate that cropping failed
    }
    cropped <- cropPageReturns[[1]] # magick image of full cropped page
    # image_write(cropped, path = paste0(pageNum, "cropped.pdf"), format = "pdf")
    returnList <- list(cropPageReturns[[2]]) # state for all data on this page
    # CROP BY COUNTY SUMMARY TABLES                                  question: also want to handle state summary tables?
    imgList <- findAndCrop(cropped, "COUNTY TOTALS")
    if (length(imgList) == 1 & is.null(imgList[[1]])) { # fix this smol issue
        print("no county summary tables on current page!")
        imgList <- list(cropped)
    } else {
        print(paste0("removed county summary tables, data from ", length(imgList), " counties remain"))
    }
    # CROP COLUMNS FOR EACH IMAGE
    for (i in 1:length(imgList)) {
        #image_write(imgList[[i]], path = paste0(pdfName, "/table", i, ".pdf"), format = "pdf")
        columns <- vector("character", 6) # will hold OCR-extracted text for each column
        width <- as.numeric(image_info(imgList[[i]])[2]) # set width and height for cropped page
        height <- as.numeric(image_info(imgList[[i]])[3])
        col_widths <- c(0, 0, 0, 0, 0, 0) # numeric vector to store pixel widths for each column
        for (j in 1:length(col_pct)) {
            col_widths[j] <- col_pct[j] * width
            # print(col_widths)
            if (j == length(col_pct)) {
                geo <- paste0(width - sum(col_widths[1:j-1]), 'x', height, '+', sum(col_widths[1:j-1]), '+', 0)
            } else {
                geo <- paste0(col_widths[j], 'x', height, '+', sum(col_widths[1:j-1]), '+', 0)
            }
            #geo <- paste0(col_widths[j], 'x', height, '+', sum(col_widths[1:j-1]), '+', 0)
            column <- image_crop(imgList[[i]], geo)
            #image_write(column, path = paste0(pageNum, "col", j, ".pdf"), format = "pdf")
            columns[j] <- ocr(column, engine = tesseract("eng"))
        }
        returnList <- c(returnList, list(columns))
    }
    return(returnList)
}
