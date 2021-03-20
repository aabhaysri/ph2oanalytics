#importing necessary libraries
library("tidyverse")
library("ggpubr")

#create a list of all the files in the working directory
files <- list.files(pattern=".*csv") 

#for loop iterates over the list and reads the csvs in the list
for(file in files){
  #used paste0 to obtain the name of the file. Removed extra info using substring.
  fileName <- paste0(str_sub(paste0(file), 1, nchar(paste0(file))-8), ".png")
  
  #read and filter the data
  df <- read.csv(file)
  colnames(df)[3] = "time"
  date <- df %>%
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
  
  #selecting and filtering the pH data and reassigning it into the "dataframe"
  df <- df %>%
    select(c(5))
  colnames(df) <- "pH"
  df <- filter(df, !(pH == ""), !(pH == "14n"))
  
  #"unlisting" the df and converting the pH values into numerics.
  phVal <- as.numeric(as.character(unlist(df)))
  #increment
  inc <- seq_along(phVal)
  df <- as.data.frame(phVal)
  #add the increment column to the dataframe.
  df <- cbind(df, inc)
  #extracting the name of file
  file <- paste0(file)
  
  #creating a plot
  plot <- ggplot(data=df, aes(x=inc, y=phVal)) +
    geom_smooth(color="black", alpha = .7) +
    geom_smooth(method = "lm") +
    stat_regline_equation(aes(label =  paste(..eq.label..))) +
    labs(title="pH Change Over Time", 
         subtitle="pH2O Analytics", 
         caption=str_sub(file, 1, nchar(file)-8), 
         x=paste0("Time ", paste0("(Inc = ", time, "s)
  First Date:", t1)), 
         y="pH Level") +
    theme_light() +
    theme(
      plot.title = element_text(color = "black", face = "bold"),
      plot.subtitle = element_text(color = "green", size=10),
      plot.caption = element_text(color = "black", size=6),
      axis.title.x = element_text(color = "black", size=8),
      axis.title.y = element_text(color = "black", size=10)
    )
  
  #saving the file to a png and forwarding it to google drive
  sendfile <- ggsave(fileName, width=16, height=9, units="cm")
  drive_upload(fileName, path="~/Data Sets - pH2O Analytics/Datasets/Models/", name=fileName)
}

