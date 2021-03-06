#install.packages("pdftools")
#install.packages("magick")
#install.packages("tesseract")
library(pdftools)
library(magick)
library(tesseract)

########################################################################################
#  Script to brainstorm/test procedure for parsing ocr text and putting data in a dataframe
#  Imports testPDF2, crops into columns, reads columns with OCR, and parses data
#  testPDF2 contains a county break and summary stats in the middle of a page
########################################################################################

col_widths <- c(1800, 740, 425, 580, 465, 585)    # dimensions for cropping
columns <- vector("character", 6)                 # will hold OCR-extracted text for each column

img <- image_read_pdf("DataParsing/testPDF2.pdf", density = 600)
width <- image_info(img)[2]
height <- image_info(img)[3]                 # would usually be 1559
crop_geo <- paste0(width - 391, 'x', height - 4950, '+', 380, '+', 880) # crop geometry (type '?image_crop' for details)
cropped <- image_crop(img, crop_geo)
image_write(cropped, path = paste0("DataParsing/cropped.pdf"), format = "pdf") # saved cropped page to folder
width <- image_info(cropped)[2] # reset width and height for cropped page
height <- image_info(cropped)[3]
for (j in 1:length(col_widths)) {
    geo <- paste0(col_widths[j], 'x', height , '+', sum(col_widths[1:j]) - col_widths[j], '+', 0)
    column <- image_crop(cropped, geo)
    columns[j] <- ocr(column, engine = tesseract("eng"))
    # image_write(column, path = paste0("DataParsing/col", j, ".pdf"), format = "pdf")
}

splitColumns <- strsplit(columns, '\n')         # list of vectors containing split column data
splitColumnsOrig <- splitColumns                # will keep track of how data originally looked

# extract and identify county, delete those rows from col1
indexOfParen <- grep("(", splitColumns[[1]], fixed = TRUE)
county <- strsplit(splitColumns[[1]][indexOfParen], " ")[[1]][1]
print(county)                 # put this in appropriate place in final dataframe!!
deleteRows <- c(rep(FALSE, 4), rep(TRUE, length(splitColumns[[1]]) - 4))
splitColumns[[1]] <- splitColumns[[1]][deleteRows]

# delete unneccessary whitespace data
for (i in 1:length(col_widths)) {
    col <- splitColumns[[i]]
    for (j in 1:length(col)) {
        whiteSpaces <- col == ""
    }
    splitColumns[[i]] <- splitColumns[[i]][!whiteSpaces]
}

# all column data vectors need to have same length before putting in dataframe
greatestLength <- 0
greatestLengthOrig <- 0
for (i in 1:length(col_widths)) {        # identify the column with the greatest length
    len <- length(splitColumns[[i]])
    # print(paste0(i, ": ", len))
    if (len > greatestLength) {
        greatestLength <- len
    }
    len <- length(splitColumnsOrig[[i]])
    # print(paste0(i, ": ", len))
    if (len > greatestLengthOrig) {
        greatestLengthOrig <- len
    }
}
stringData <- data.frame(Temp = 1:greatestLength)
stringDataOrig <- data.frame(Temp = 1:greatestLengthOrig)
for (i in 1:length(col_widths)) {
    col = splitColumns[[i]]
    colOrig = splitColumnsOrig[[i]] 
    if (length(col) < greatestLength) {
        col <- c(col, rep('#', greatestLength - length(col))) # add '#' characters to fill in
    }
    if (length(colOrig) < greatestLengthOrig) {
        colOrig <- c(colOrig, rep('#', greatestLengthOrig - length(colOrig))) # add '#' characters to fill in
    }
    stringData[paste0("Col", i)] <- col
    stringDataOrig[paste0("Col", i)] <- colOrig
}
stringData <- stringData[-1]
stringDataOrig <- stringDataOrig[-1]

print("------------------ Original Data --------------------")
print(stringDataOrig)
print("------------------ Cleaned Data --------------------")
print(stringData)
