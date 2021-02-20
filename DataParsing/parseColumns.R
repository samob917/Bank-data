#install.packages("pdftools")
#install.packages("magick")
#install.packages("tesseract")
library(pdftools)
library(magick)
library(tesseract)

########################################################################################
#  Script to brainstorm/test procedure for parsing ocr text and putting data in a dataframe
#    
#    
########################################################################################

col_widths <- c(1800, 740, 425, 580, 465, 585)

img <- image_read_pdf("DataParsing/testPDF.pdf", density = 600)
width <- image_info(img)[2]
height <- image_info(img)[3]
crop_geo <- paste0(width - 391, 'x', height - 1559, '+', 390, '+', 880) # crop geometry (type '?image_crop' for details)
cropped <- image_crop(img, crop_geo)
width <- image_info(cropped)[2] # reset width and height for cropped page
height <- image_info(cropped)[3]
columns <- vector("character", 7)
for (j in 1:length(col_widths)) {
    geo <- paste0(col_widths[j], 'x', height , '+', sum(col_widths[1:j]) - col_widths[j], '+', 0)
    column <- image_crop(cropped, geo)
    columns[j] <- ocr(column, engine = tesseract("eng"))
    image_write(column, path = paste0("DataParsing/col", j, ".pdf"), format = "pdf")
}

print(columns[1])
col1 = strsplit(columns[1], '\n')
print(col1)
# TODO
# Create dataframe with the 6 columns of strings, just to see them side by side
# will help in making sure columns are kept in alignment when we delete rows


"
create a dataframe to store data
Case 1: regular data, no county breaks
for each line:
    convert data in each column into appropriate type, store in dataframe
    move to next line in all columns

Case 2: county change and summary statistics

use regex to search for:  (xxx)
if found:
    find the next set of parenthesis (to show where this county data ends)
    delete row above and below parenthesis
    pull county name from line with parenthesis in it, fill in dataframe
    





"

