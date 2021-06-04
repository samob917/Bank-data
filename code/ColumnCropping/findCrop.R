#install.packages("tesseract")
#install.packages("magick")
library(tesseract)
library(magick)


########################################################################################
#  Script containing findAndCrop function, which takes in a border-cropped page and splits
#  the page into horizontal chunks based on a phrase to search for
#  to call in another file:   source("findCrop.R)
########################################################################################

# procedure: 
#     find pixel locations of all occurrences of phrase on page
#     for each instance:
#       crop out the section above the phrase
#       add cropped section to return list
#     add (pixel?) offset to location of phrase, begin next search
#     alternative: rather than a fixed offset, could make this specific to county tables
#                  and search for the start of the next county ()
#                  but I feel like there's enough wiggle room that a fixed pixel offset is fine


# PARAMETERS
#   img - the magick img file of the full cropped table
#   phrase - the string containing the phrase to search for
# RETURNS
#  a list containing the horizontally cropped images


SUMMARY_TABLE_HEIGHT <- 450 # height of county summary table, in pixels

findAndCrop <- function(img, phrase) {
    fullText <- ocr(img, engine = tesseract("eng"))
    fullText <- strsplit(fullText, "\n")[[1]]
    countyTables <- grep(phrase, fullText)
    numTables <- length(countyTables)
    if (numTables == 0) {
        return(list(NULL))
    }
    width <- as.numeric(image_info(img)[2])
    height <- as.numeric(image_info(img)[3])
    # print(paste0("width: ", width, "  height: ", height))
    # split whole image into top and bottom halves
    geo <- paste0(width, 'x', height/2, '+', 0, '+', 0)
    topHalf <- image_crop(img, geo)
    geo <- paste0(width, 'x', height/2, '+', 0, '+', height/2)
    botHalf <- image_crop(img, geo)
    topText <- ocr(topHalf, engine = tesseract("eng"))
    topText <- strsplit(topText, "\n")[[1]]
    botText <- ocr(img, engine = tesseract("eng"))
    botText <- strsplit(botText, "\n")[[1]]
    numTablesTop <- length(grep(phrase, topText))
    numTablesBot <- length(grep(phrase, botText))
    imgToSearch <- NULL
    botHalfIndicator <- FALSE
    if (numTablesTop + numTablesBot != numTables) { # maybe "COUNTY TOTALS" was on the halfway line, 
        #                                           search whole page? would be inefficient but a rare case
        imgToSearch <- img
    } else if (numTablesTop == 0) { # case that all tables are in bottom half
        imgToSearch <- botHalf
        height <- height/2
        botHalfIndicator <- TRUE
        print("searching bottom half for county summary table")
    } else if (numTablesBot == 0) { # case that all tables are in top half
        imgToSearch <- topHalf
        height <- height/2
        print("searching top half for county summary table")
    } else { # case that there are tables in both top and bottom halves, will search whole image
        imgToSearch <- img
    }
    
    yCursor <- 0 # will iterate down imgToSearch to find county tables
    tableLocations <- vector(mode = "numeric", length = numTables)
    for (i in 1:numTables) {
        # identify location of county table
        while (yCursor < height) {
            strip_geo <- paste0(width, 'x', 60, '+', 0, '+', yCursor) # crop geometry (type '?image_crop' for details)
            strip <- image_crop(imgToSearch, strip_geo)
            text <- ocr(strip, engine = tesseract("eng"))
            if (grepl(phrase, text, fixed = TRUE) == TRUE) {
                break
            }
            yCursor <- yCursor + 20
        } # now assuming yCursor points to the top of "COUNTY TOTALS"
        tableLocations[i] <- yCursor
        if (botHalfIndicator == TRUE) {
            tableLocations[i] <- tableLocations[i] + height
        }
        yCursor <- yCursor + SUMMARY_TABLE_HEIGHT 
    }
    # print("table locations: ")
    # print(tableLocations)
    imgList <- vector(mode = "list", length = numTables) # return list of images, will append last image if not blank
    top <- 0  # will track y-coord where current image should start
    for (i in 1:numTables) {
        yDim <- tableLocations[[i]]
        geo <- paste0(width, 'x', yDim - top, '+', 0, '+', top) # crop geometry (type '?image_crop' for details)
        cropped <- image_crop(img, geo)
        if (!(i == 1 & ocr(cropped, engine = tesseract("eng")) == "")) { # check that section above first county table isn't blank
            imgList[[i]] <- cropped 
        }
        top <- yDim + SUMMARY_TABLE_HEIGHT
    } # check that section after final county table isn't blank
    geo <- paste0(width, 'x', as.numeric(image_info(img)[3]) - top, '+', 0, '+', top) # crop geometry (type '?image_crop' for details)
    cropped <- image_crop(img, geo)    
    text <- ocr(cropped, engine = tesseract("eng"))
    if (text != "") {
        imgList <- c(imgList, cropped)
    }
    if (is.null(imgList[[1]])) {
        return(imgList[-1]) # case that county table was at top of page, first image was blank
    }
    #image_write(imgList[[i]], path ="findAndCrop/test.pdf", format = "pdf")
    return(imgList)
}


