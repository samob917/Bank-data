library(pdftools)
library(magick)
library(tesseract)

# put the paths to the test files in lines 8, 17, 28

testPDF <- function(density = 300) {
    img <- image_read_pdf("formats/testPDF.pdf", density = density)
    width <- image_info(img)[2]
    height <- image_info(img)[3]
    crop_geo <- paste0(width/2, 'x', height/2, '+', width/4, '+', height/4)
    cropped <- image_crop(img, crop_geo)
    text <- ocr(cropped, engine = tesseract("eng"))
}

testPDFmulti <- function(density = 300) {
    img <- image_read_pdf("formats/testPDFmulti.pdf", density = density)
    for (page in 1:length(img)) {
        width <- image_info(img)[page, 2]
        height <- image_info(img)[page, 3]
        crop_geo <- paste0(width/2, 'x', height/2, '+', width/4, '+', height/4)
        cropped <- image_crop(img[page], crop_geo)
        text <- ocr(cropped, engine = tesseract("eng"))
    }
}

testPDFtoPNG <- function() {
    pngName <- pdf_convert("formats/testPDF.pdf", verbose = FALSE)
    img <- image_read(pngName)
    width <- image_info(img)[2]
    height <- image_info(img)[3]
    crop_geo <- paste0(width/2, 'x', height/2, '+', width/4, '+', height/4)
    cropped <- image_crop(img, crop_geo)
    text <- ocr(cropped, engine = tesseract("eng"))
}

testPDFtoPNGmulti <- function() {
    pngName <- pdf_convert("formats/testPDFmulti.pdf", verbose = FALSE)
    img <- image_read(pngName)
    for (pageNum in 1:length(img)) {
        width <- image_info(img)[pageNum, 2]
        height <- image_info(img)[pageNum, 3]
        crop_geo <- paste0(width/2, 'x', height/2, '+', width/4, '+', height/4)
        cropped <- image_crop(img[pageNum], crop_geo)
        text <- ocr(cropped, engine = tesseract("eng"))
    }
}

testJPG <- function() {
    img <- image_read("formats/testJPG.jpg")
    width <- image_info(img)[2]
    height <- image_info(img)[3]
    crop_geo <- paste0(width/2, 'x', height/2, '+', width/4, '+', height/4)
    cropped <- image_crop(img, crop_geo)
    text <- ocr(cropped, engine = tesseract("eng"))
}

testJPGmulti <- function() {
    pages <- list.files("formats/testJPGmulti")
    for (page in pages) {
        img <- image_read(paste0("formats/testJPGmulti/", page))
        width <- image_info(img)[2]
        height <- image_info(img)[3]
        crop_geo <- paste0(width/2, 'x', height/2, '+', width/4, '+', height/4)
        cropped <- image_crop(img, crop_geo)
        text <- ocr(cropped, engine = tesseract("eng"))
    }
}



print("PDF -----------------------------")
print(system.time(testPDF()))
print("PDFtoPNG ------------------------")
print(system.time(testPDFtoPNG()))
print("JPG ----------------------------")
print(system.time(testJPG()))
print("PDF Multi --------------------------")
print(system.time(testPDFmulti()))
print("PDFtoPNG Multi --------------------")
print(system.time(testPDFtoPNGmulti()))
print("JPG Multi ------------------------")
print(system.time(testJPGmulti()))

print()
print("converting PDF->PNG is most efficient")

