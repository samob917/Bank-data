#install.packages("tesseract")
#install.packages("magick")
library(tesseract)
library(magick)

source("findAndCrop/findCrop.R")
source("findAndCrop/cropPage.R")

########################################################################################
#  Driver program to test findCrop function
#  
#  to call:   ensure working directory is findCrop
#      source("main.R")
#      main()
#    
########################################################################################

main <- function(pdfName, pageNum) {
    img <- image_read_pdf(pdfName, density = 600)
    cropPageReturns <- cropPage(img, pageNum)
    table <- cropPageReturns[[1]]
    imgList <- findAndCrop(table, "COUNTY TOTALS")
    if (length(imgList) == 1 & is.null(imgList[[1]])) { # fix this smol issue
        print("no county summary tables here!")
    } else {
        for (i in 1:length(imgList)) {
            # print(imgList[[i]])
            image_write(imgList[[i]], path = paste0(pdfName, "/table", i, ".pdf"), format = "pdf")
        }
    }
    
    
    
}



