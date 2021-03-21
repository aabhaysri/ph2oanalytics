#importing necessary libraries
library("tidyverse")
library("DescTools")
library("googledrive")
library("devtools")

#creating list of files
files <- list.files(pattern=".*csv")

#creating a dataframe with 7 columns and the the length of files rows. 
df1 <- data.frame(matrix(ncol=7, nrow=length(files)))

#assigning column names
colnames(df1) <- c("Name", "Mean", "Standard Deviation", "Variance", "Mode", 'Median', "IQR")

#for loop iterating over files
for(r in 1:length(files)){
  df <- read.csv(files[r]) %>%
    select(5, 6)
  colnames(df) <- c("pH", "ap")
  df <- filter(df, !(pH == ""), !(pH == "14n"), !(ap == "P")) %>%
    select(1)
  
  phVal <- as.numeric((unlist(df)))
    df1[r, 1] = gsub(",.*", "", paste0(files[r]))
    df1[r, 2] = mean(phVal)
    df1[r, 3] = sd(phVal)
    df1[r, 4] = var(phVal)
    df1[r, 5] = Mode(phVal)
    df1[r, 6] = median(phVal)
    df1[r, 7] = IQR(phVal)
}

#creating a csv file out of df1
write.csv(df1, "Model Statistics.csv")
drive_upload("Model Statistics.csv", path="~/Data Sets - pH2O Analytics/Datasets/Models/", name="Model Statistics")
