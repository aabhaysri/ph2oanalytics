#importing necessary libraries
library("tidyverse")
library("ggpubr")
library("googledrive")
library("DescTools")

#create a list of all the files in the working directory
files <- list.files(pattern = ".*csv")

#creating a dataframe with 10 columns and the the length of files rows.
stats <- data.frame(matrix(ncol = 10, nrow = length(files)))
colnames(stats) <-
  c(
    "Name",
    "Mean",
    "Standard Deviation",
    "Variance",
    "Mode",
    'Median',
    "IQR",
    "EST pH in 5 Years",
    "EST pH in 10 Years",
    "Change in 5 Years"
  )

#calculates difference in time
timeDiff <- function(x) {
  t1 <- x[1, 3]
  t2 <- x[2, 3]
  if (str_count(t1, pattern = ":") == 1) {
    t1 <- paste0(t1, ":00")
    t2 <- paste0(t2, ":00")
  }
  t1 <-
    as.POSIXct(t1, format = "%m/%d/%Y %H:%M:%S", tz = Sys.timezone())
  t2 <-
    as.POSIXct(t2, format = "%m/%d/%Y %H:%M:%S", tz = Sys.timezone())
  change <- abs(as.numeric(difftime(t1, t2, units = "secs")))
  return(change)
}

#gets lm coefficients, z defines what the function will return
getCof <- function(x, y, z) {
  #creating statistics
  linear <- lm(y ~ x)
  cof <- coef(linear)
  splitString <- as.character(cof) %>%
    str_split(" ")
  yint <- as.numeric(splitString[1])
  m <- as.numeric(splitString[2])
  if (z == 1) {
    return(yint + m / time * 157788000)
  } else if (z == 2) {
    return(yint + m / time * 157788000 * 2)
  } else{
    return(m / time * 157788000)
  }
}

#for loop iterates over the list and reads the csvs in the list
for (r in 1:length(files)) {
  #used paste0 to obtain the name of the file. Removed extra info using gsub.
  fileName <- gsub(",.*", "", paste0(files[r]))
  
  #read and filter the data
  df <- date <- read.csv(files[r])
  
  colnames(date)[3] = "time"
  colnames(date)[6] = "ap"
  
  #filters date-time data
  date <- date %>%
    filter(!(time == ""), !(time == "20d"), !(ap == "P"))
  
  time <- timeDiff(date)
  
  #"unlisting" the df and converting the pH values into numeric.
  df <- df %>%
    select(5, 6)
  colnames(df) <- c("pH", "ap")
  phVal <- df %>%
    filter(!(pH == ""), !(pH == "14n"),!(ap == "P")) %>%
    select(1) %>%
    unlist() %>%
    as.numeric()
  
  #increment
  inc <- seq_along(phVal)
  df <- as.data.frame(phVal) %>%
    cbind(inc)
  
  #fills the stats dataframe with stats
  stats[r, 1] = fileName
  stats[r, 2] = mean(phVal)
  stats[r, 3] = sd(phVal)
  stats[r, 4] = var(phVal)
  stats[r, 5] = Mode(phVal)
  stats[r, 6] = median(phVal)
  stats[r, 7] = IQR(phVal)
  stats[r, 8] = getCof(inc, phVal, 1)
  stats[r, 9] = getCof(inc, phVal, 2)
  stats[r, 10] = getCof(inc, phVal, 3)
  
  #creating a plot
  plot <- ggplot(data = df, aes(x = inc, y = phVal)) +
    
    geom_smooth(color = "black",
                alpha = .7) +
    
    geom_smooth(method = "lm") +
    
    stat_regline_equation(aes(label =  paste(..eq.label..))) +
    
    labs(
      title = "pH Change Over Time",
      subtitle = "pH2O Analytics",
      caption = fileName,
      x = paste0("Time ", paste0("(Inc = ", time, "s)
  First Date:", date[1, 3])),
      y = "pH Level"
    ) +
    
    theme_light() +
    
    theme(
      plot.title = element_text(color = "black", face = "bold"),
      plot.subtitle = element_text(color = "green", size = 10),
      plot.caption = element_text(color = "black", size = 6),
      axis.title.x = element_text(color = "black", size = 8),
      axis.title.y = element_text(color = "black", size = 10)
    )
  
  #saving the file to a png and forwarding it to google drive
  sendfile <- ggsave(
    paste0(fileName, ".png"),
    width = 16,
    height = 9,
    units = "cm"
  )
  drive_upload(paste0(fileName, ".png"),
               path = "~/Data Sets - pH2O Analytics/Datasets/Models/",
               name = fileName)
  
}

#writing stats to a csv
write.csv(stats, "Model Statistics.csv")
drive_upload(
  "Model Statistics.csv",
  path = "~/Data Sets - pH2O Analytics/Datasets/Models/",
  name = "Model Statistics",
  overwrite = TRUE
)
