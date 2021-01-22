install.packages("tesseract")
install.packages("magick")
install.packages("staplr")
library(tesseract)
library(magick)
library(staplr)


col_widths <- c(1800, 740, 425, 580, 465, 585)

if (!file.exists("pages")) {
    dir.create("pages")
}
split_pdf("1993data.pdf", output_directory = "pages")
file.remove("pages/doc_data.txt")
pages <- list.files("pages")
for (i in 1:length(pages)) {
    # read in image
    image <- image_read_pdf(paste0("pages/", pages[i]), density = 600)
    print(image_info(image))
    # perform initial crop
    width <- image_info(image)[2]
    height <- image_info(image)[3]
    crop_geo <- paste0(width - 391, 'x', height - 1559, '+', 390, '+', 880)
    cropped <- image_crop(image, crop_geo)
    # save cropped page in folder for this page
    if (!file.exists(as.character(i))) {
        dir.create(as.character(i))
    }
    image_write(cropped, path = paste0(i, "/cropped.pdf"), format = "pdf")
    # crop out individual columns
    width <- image_info(cropped)[2]
    height <- image_info(cropped)[3]
    for (j in 1:length(col_widths)) {
        geo <- paste0(col_widths[j], 'x', height , '+', sum(col_widths[1:j]) - col_widths[j], '+', 0)
        column <- image_crop(cropped, geo)
        # save columns in folder for this page
        image_write(column, path = paste0(i, "/col", j, ".pdf"), format = "pdf")
    }
}







