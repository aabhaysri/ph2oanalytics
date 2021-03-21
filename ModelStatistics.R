#importing necessary libraries
library("tidyverse")
library("DescTools")
library("googledrive")
library("devtools")

#creating list of files
files <- list.files(pattern=".*csv")

#creating a dataframe with 9 columns and the the length of files rows. 
df1 <- data.frame(matrix(ncol=9, nrow=length(files)))

#assigning column names
colnames(df1) <- c("Name", "Mean", "Standard Deviation", "Variance", "Mode", 'Median', "IQR", "pH in 5 Years", "Change")

#for loop iterating over files
for(r in 1:length(files)){
  df2 <- df <- read.csv(files[r])
  df <- select(df, 5, 6)
  colnames(df) <- c("pH", "ap")
  df <- filter(df, !(pH == ""), !(pH == "14n"), !(ap == "P")) %>%
    select(1)
  
  phVal <- as.numeric((unlist(df)))
  inc <- seq_along(phVal)
  
  #filter data
  colnames(df2)[3] = "time"
  date <- df2 %>%
    filter(!(time == ""), !(time == "20d"))
  
  #time variables let us know the increment
  t1 <- date[1, 3]
  t2 <- date[2, 3]
  #standardizing the time
  if(str_count(t1, pattern=":") == 1){
    t1 <- paste0(t1, ":00")
    t2 <- paste0(t2, ":00")
  }
  #converting the time characters into a Date-Time object
  t1 <- as.POSIXct(t1,format="%m/%d/%Y %H:%M:%S",tz=Sys.timezone())
  t2 <- as.POSIXct(t2,format="%m/%d/%Y %H:%M:%S",tz=Sys.timezone())
  time <- as.character(abs(as.numeric(difftime(t1, t2, units = "secs"))))
  
  linear <- lm(phVal ~ inc)
  cof <- coef(linear)
  splitString <- as.character(cof) %>%
    str_split(" ")
  yint <- as.numeric(splitString[1])
  m <- as.numeric(splitString[2])
  predictFive <- yint + (m/as.numeric(time) * 157788000)
  change <- m/as.numeric(time) * 157788000
  
  df1[r, 1] = gsub(",.*", "", paste0(files[r]))
  df1[r, 2] = mean(phVal)
  df1[r, 3] = sd(phVal)
  df1[r, 4] = var(phVal)
  df1[r, 5] = Mode(phVal)
  df1[r, 6] = median(phVal)
  df1[r, 7] = IQR(phVal)
  df1[r, 8] = predictFive
  df1[r, 9] = change

}

#creating a csv file out of df1
write.csv(df1, "Model Statistics.csv")
drive_upload("Model Statistics.csv", path="~/Data Sets - pH2O Analytics/Datasets/Models/", name="Model Statistics")
