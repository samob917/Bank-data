


main <- function(pdfName, firstPage, firstCounty) {
    image <- image_read_pdf(paste0("test/", pdfName), density = 600)
    pageNum <- firstPage
    state <- ""
    county <- firstCounty
    finalDF <- null
    
    for (page in 1:length(image)) {
        cropColumns <- cropColumns(image[page], pageNum)
        
        state <- cropColumns[[2]]
        columns <- cropColumns[[1]]
        
        splitColumns <- strsplit(columns, '\n')         # list of vectors containing split column data
        #splitColumnsOrig <- splitColumns                # will keep track of how data originally looked
        
        
        
        
        
    }
}