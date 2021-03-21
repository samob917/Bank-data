#install.packages("tesseract")
#install.packages("magick")
library(tesseract)
library(magick)

########################################################################################
#  Imports 7-page PDF 1993data.pdf, iterates though each page and crops out 6 columns
#    using predetermined pixel counts. Saves cropped columns as PDFs in folders for
#    each page (ie ColumnCropping/Page1/col1.pdf)
########################################################################################


#################### Using smart border-cropping ##############################

source("ColumnCropping/cropPage.R")

# stores percentage that each column should take up
col_pct <- c(0.36643, 0.15765, 0.09054, 0.12356, 0.09906, 0.04261)


pageNum <- 1

for (page in 1:length(image)) {   # for each page in the PDF
    # INITIAL CROP
    cropPageReturns <- cropPage(image[page], pageNum)
    cropped <- cropPageReturns[[1]]
    if (!file.exists(paste0("ColumnCropping/Page", page))) { # create a folder to hold crops for this page
        dir.create(paste0("ColumnCropping/Page", page))
    }
    image_write(cropped, path = paste0("ColumnCropping/Page", page, "/cropped.pdf"), format = "pdf") # saved cropped page to folder
    width <- as.numeric(image_info(cropped)[2]) # reset width and height for cropped page
    height <- as.numeric(image_info(cropped)[3])
    # CROP COLUMNS
    col_widths <- c(0, 0, 0, 0, 0, 0) # numeric vector to store pixel widths for each column
    for (j in 1:length(col_pct)) {
        col_widths[j] <- col_pct[j] * width
        # print(col_widths)
        if (j == length(col_pct)) {
            geo <- paste0(width - sum(col_widths[1:j-1]), 'x', height, '+', sum(col_widths[1:j]) - col_widths[j], '+', 0)
        } else {
            geo <- paste0(col_widths[j], 'x', height, '+', sum(col_widths[1:j]) - col_widths[j], '+', 0)
        }
        column <- image_crop(cropped, geo)
        image_write(column, path = paste0("ColumnCropping/Page", page, "/col", j, ".pdf"), format = "pdf")
    }
    pageNum <- pageNum + 1
}

'

col_widths <- c(1800, 740, 425, 580, 465, 585) # numeric vector storing pixel widths for each column

image <- image_read_pdf("ColumnCropping/1993B1_1-7.pdf", density = 600) # read in PDF
print(image_info(image))
for (page in 1:length(image)) {   # for each page in the PDF
    # INITIAL CROP
    width <- image_info(image)[page, 2]
    height <- image_info(image)[page, 3]
    crop_geo <- paste0(width - 390, "x", height - 1560, "+", 390, "+", 880) # crop geometry (type "?image_crop" for details)
    cropped <- image_crop(image[page], crop_geo)
    if (!file.exists(paste0("ColumnCropping/Page", page))) { # create a folder to hold crops for this page
        dir.create(paste0("ColumnCropping/Page", page))
    }
    image_write(cropped, path = paste0("ColumnCropping/Page", page, "/cropped.pdf"), format = "pdf") # saved cropped page to folder
    width <- image_info(cropped)[2] # reset width and height for cropped page
    height <- image_info(cropped)[3]
    # CROP COLUMNS
    for (j in 1:length(col_widths)) {
        geo <- paste0(col_widths[j], "x", height , "+", sum(col_widths[1:j]) - col_widths[j], "+", 0)
        column <- image_crop(cropped, geo)
        image_write(column, path = paste0("ColumnCropping/Page", page, "/col", j, ".pdf"), format = "pdf")
    }
}

'





