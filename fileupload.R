library("tidyverse")
library("googledrive")
library("devtools")
files <- list.files(pattern=".*csv") 

for(file in files){
  fileName <- paste0(file)
  drive_upload(file, path="~/Data Sets - pH2O Analytics/Datasets/Cleansed Data/", name=fileName)
}