'''
This code splits a pdf into 25 page blocks

'''
from PyPDF2 import PdfFileWriter, PdfFileReader

inputpdf = PdfFileReader(open("modified_1993_book6.pdf", "rb"))
output = PdfFileWriter()

num_pages = inputpdf.numPages


page_num = 1
output.addPage(inputpdf.getPage(0))
while page_num < num_pages:
    if page_num % 25 == 0:
        with open("1993_book6_toPG{}.pdf".format(page_num), "wb") as outputstream:
                output.write(outputstream)
        del output
        output = PdfFileWriter()
        output.addPage(inputpdf.getPage(page_num))
        page_num += 1
        
        
    else:
        output.addPage(inputpdf.getPage(page_num))
        page_num += 1   

with open("1993_book6_final.pdf", "wb") as outputstream:
    output.write(outputstream)