#install.packages("pdftools")
#install.packages("magick")
#install.packages("tesseract")
library(pdftools)
library(magick)
library(tesseract)

########################################################################################
#  Script to brainstorm/test procedure for parsing ocr text and putting data in a dataframe
#  Imports testPDF2, crops into columns, reads columns with OCR, and parses data
#    
########################################################################################

col_widths <- c(1800, 740, 425, 580, 465, 585)    # dimensions for cropping
columns <- vector("character", 6)                 # will hold OCR-extracted text for each column

img <- image_read_pdf("DataParsing/testPDF2.pdf", density = 600)
width <- image_info(img)[2]
height <- image_info(img)[3]                 # would usually be 1559
crop_geo <- paste0(width - 391, 'x', height - 4950, '+', 380, '+', 880) # crop geometry (type '?image_crop' for details)
cropped <- image_crop(img, crop_geo)
width <- image_info(cropped)[2] # reset width and height for cropped page
height <- image_info(cropped)[3]
for (j in 1:length(col_widths)) {
    geo <- paste0(col_widths[j], 'x', height , '+', sum(col_widths[1:j]) - col_widths[j], '+', 0)
    column <- image_crop(cropped, geo)
    columns[j] <- ocr(column, engine = tesseract("eng"))
    image_write(column, path = paste0("DataParsing/col", j, ".pdf"), format = "pdf")
}

splitColumns <- strsplit(columns, '\n')         # list of vectors containing split column data
splitColumnsOrig <- splitColumns                # will keep track of how data originally looked