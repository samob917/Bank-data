#install.packages("pdftools")
#install.packages("magick")
#install.packages("tesseract")
library(pdftools)
library(magick)
library(tesseract)


########################################################################################
#  Functions import, crop, and perform OCR on images of various formats. Functions are
#    called in system.time(), which prints the time taken to complete the function call.
#    Converting PDFs to PNGs before importing seems most efficient
########################################################################################


testPDF <- function(density = 300) {
    img <- image_read_pdf("ImageFormatTesting/formats/testPDF.pdf", density = density)
    width <- image_info(img)[2]
    height <- image_info(img)[3]
    crop_geo <- paste0(width/2, 'x', height/2, '+', width/4, '+', height/4)
    cropped <- image_crop(img, crop_geo)
    text <- ocr(cropped, engine = tesseract("eng"))
}

testPDFmulti <- function(density = 300) {
    img <- image_read_pdf("ImageFormatTesting/formats/testPDFmulti.pdf", density = density)
    for (page in 1:length(img)) {
        width <- image_info(img)[page, 2]
        height <- image_info(img)[page, 3]
        crop_geo <- paste0(width/2, 'x', height/2, '+', width/4, '+', height/4)
        cropped <- image_crop(img[page], crop_geo)
        text <- ocr(cropped, engine = tesseract("eng"))
    }
}

testPDFtoPNG <- function() {
    pngName <- pdf_convert("ImageFormatTesting/formats/testPDF.pdf", verbose = FALSE)
    img <- image_read(pngName)
    width <- image_info(img)[2]
    height <- image_info(img)[3]
    crop_geo <- paste0(width/2, 'x', height/2, '+', width/4, '+', height/4)
    cropped <- image_crop(img, crop_geo)
    text <- ocr(cropped, engine = tesseract("eng"))
    file.remove(pngName)
}

testPDFtoPNGmulti <- function() {
    pngName <- pdf_convert("ImageFormatTesting/formats/testPDFmulti.pdf", verbose = FALSE)
    img <- image_read(pngName)
    for (pageNum in 1:length(img)) {
        width <- image_info(img)[pageNum, 2]
        height <- image_info(img)[pageNum, 3]
        crop_geo <- paste0(width/2, 'x', height/2, '+', width/4, '+', height/4)
        cropped <- image_crop(img[pageNum], crop_geo)
        text <- ocr(cropped, engine = tesseract("eng"))
    }
    file.remove(pngName)
}

testJPG <- function() {
    img <- image_read("ImageFormatTesting/formats/testJPG.jpg")
    width <- image_info(img)[2]
    height <- image_info(img)[3]
    crop_geo <- paste0(width/2, 'x', height/2, '+', width/4, '+', height/4)
    cropped <- image_crop(img, crop_geo)
    text <- ocr(cropped, engine = tesseract("eng"))
}

testJPGmulti <- function() {
    pages <- list.files("ImageFormatTesting/formats/testJPGmulti")
    for (page in pages) {
        img <- image_read(paste0("ImageFormatTesting/formats/testJPGmulti/", page))
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

print("--------------------------------------")
print("converting PDF->PNG is most efficient")

