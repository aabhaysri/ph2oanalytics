#importing libraries
library("tidyverse")
library("googledrive")
library("devtools")

#making list of files
files <- list.files(pattern=".*csv") 


#for loop to upload the files to gdrive
for(file in files){
  fileName <- paste0(file)
  drive_upload(file, path="~/Data Sets - pH2O Analytics/Datasets/Cleansed Data/", name=fileName)
}
