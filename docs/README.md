Helpful info for this project


#######################################################################
Git instructions

Terminology
	local repository - folder of files on your computer ("Bank-data")
	remote repository - version of files stored on github

Before you do anything else, pull the changes from the remote repository to your local repository
	git pull

To see the status of your repository and files:
	git status

To push your edits from your local repo to the remote repo
	git add fileName.R (for each file you edited)
	git commit -m "descriptive message about change to fileName.R"
	git push
	


#######################################################################
How to run R code from this repository

1) Pull changes to your local repository (git pull)
2) In RStudio, open the project (Bank-data.Rproj)
	In the terminal in RStudio, call getwd()
	Should be Bank-data
3) From the "Files" section in RStudio, open the R script you want to run
4) Note the comments at the top of the file describing what the code does
5) If it's your first time running this script, uncomment all of the "install.packages()"
     commands at the top of the file
     	Re-comment them after you run it, so you don't keep re-installing with every run
6) To run the file, click "Source"




#######################################################################
How to write R code to this repository

1) Create a folder that will hold all code and necessary files (use ColumnCropping as an example)
2) Put R scripts at the top level of that folder
3) In each R file:
	At the top, install and load all necessary packages by calling install.packages() and library()
	Comment out the install.packages() commands
3) Put a multi-line comment explaining the high-level purpose of this file
