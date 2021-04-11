#install.packages("pdftools")
#install.packages("magick")
#install.packages("tesseract")
library(pdftools)
library(magick)
library(tesseract)

# TODO write code to crop further edge!

########################################################################################
#  Script containing cropPage function, which performs smart border cropping on any page
#  to call in another file:   source("cropPage.R)
#    
########################################################################################

#   Function to crop the data table out of the given page
#   Parameters:
#       img - the magick image file (the return value of image_read_pdf)
#       page - the page number as a string
#   Returns
#       a list
#           1st element is the magick image file of the cropped page
#           2nd element is the state (as a string)
#
cropPage <- function(img, page) {
    width <- image_info(img)[2]
    height <- image_info(img)[3]                 
    crop_geo <- paste0(width*.94, 'x', height*0.93, '+', width*.06, '+', 0)
    croppedWatermark <- image_crop(img, crop_geo)
    startX <- 0
    while (startX <= image_info(croppedWatermark)[2]) {  # while startX less than width  
        strip_geo <- paste0(60, 'x', image_info(croppedWatermark)[3], '+', startX, '+', 0) # vertical strip of width 60 from startX
        strip <- image_crop(croppedWatermark, strip_geo)
        text <- ocr(strip, engine = tesseract("eng"))
        if (text != '') {
            break
        }
        startX = startX + 30
    }
    startY <- 300
    while (startY <= image_info(croppedWatermark)[3]) {  # while startY less than height  
        strip_geo <- paste0(image_info(croppedWatermark)[2], 'x', 300, '+', 0, '+', startY - 300) # horizontal strip of height 300 from startY-300
        strip <- image_crop(croppedWatermark, strip_geo)
        text <- ocr(strip, engine = tesseract("eng"))
        if (grepl("DEPOSITS DEPOSITS DEPOSITS", text, fixed = TRUE) == TRUE) {   # end of column headers
            break
        }
        startY = startY + 50
    }
    cutFromBottom <- 100
    while (cutFromBottom <= image_info(croppedWatermark)[3] - startY) {  # while cutFromBottom less than new height  
        strip_geo <- paste0(image_info(croppedWatermark)[2], 'x', 100, '+', 0, '+', image_info(croppedWatermark)[3] - cutFromBottom) # horizontal strip of height 100 from end-cutFromBottom
        strip <- image_crop(croppedWatermark, strip_geo)
        text <- ocr(strip, engine = tesseract("eng"))
        if (grepl(page, text, fixed = TRUE) == TRUE) {
            text = strsplit(text, "\n")[[1]][1] 
            state <- strsplit(text, " ")[[1]][2] 
            print(paste0("State: ", state))
            break
        }
        cutFromBottom = cutFromBottom + 40
    }
    crop_geo <- paste0(image_info(croppedWatermark)[2] - startX - 20, 'x', image_info(croppedWatermark)[3] - startY - cutFromBottom, '+', startX, '+', startY)
    finalCropped <- image_crop(croppedWatermark, crop_geo)
    returns <- list(finalCropped, state)
    return(returns)
}