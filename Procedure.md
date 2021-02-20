Procedure for data scraping (1993)


General outline
1) Read image files into R
2) Crop images into columns, preform OCR on each column
3) Parse columns, store data in dataframe



Details

1) Read image files into R
	> Original file format is a multi-page PDF
		How many pages at a time is TBD (could potentially be a whole book)
	> Convert to PNG
	> Import image into R using magick package
	
2) Crop images into columns, perform OCR on each column
	For each page of the imported image:
	> Crop whitespace/border off of whole page
	> Crop into 6 columns using pre-determined pixel counts
	For each cropped column:
	> Use Tesseract's ocr() to extract text into one giant character string
	> Split string by newlines (\n) and store individual words in a vector
	
At this point, we have 6 character vectors holding the text from each column.

3) Parse columns, store data in dataframe
