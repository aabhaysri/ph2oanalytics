library("tidyverse")
library("ggpubr")

files <- list.files(pattern=".*csv")
for(file in files){
  fileName <- paste0(str_sub(paste0(file), 1, nchar(paste0(file))-8), ".png")
  
  df <- read.csv(file)
  colnames(df)[3] = "time"
  date <- df %>%
    filter(!(time == ""), !(time == "20d"))
  
  t1 <- date[1, 3]
  t2 <- date[2, 3]
  if(str_count(t1, pattern=":") == 1){
    t1 <- paste0(t1, ":00")
    t2 <- paste0(t2, ":00")
  }
  t1 <- as.POSIXct(t1,format="%m/%d/%Y %H:%M:%S",tz=Sys.timezone())
  t2 <- as.POSIXct(t2,format="%m/%d/%Y %H:%M:%S",tz=Sys.timezone())
  time <- as.character(abs(as.numeric(difftime(t1, t2, units = "secs"))))

  df <- df %>%
    select(c(5))
  colnames(df) <- "pH"
  df <- filter(df, !(pH == ""), !(pH == "14n"))
  
  phVal <- as.numeric(as.character(unlist(df)))
  inc <- seq_along(phVal)
  df <- as.data.frame(phVal)
  df <- cbind(df, inc)
  file <- paste0(file)
  
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
  
  sendfile <- ggsave(fileName, width=16, height=9, units="cm")
  drive_upload(fileName, path="~/Data Sets - pH2O Analytics/Datasets/Models/", name=fileName)
}

