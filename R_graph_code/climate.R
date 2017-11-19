library(plotly)


#clean data
data <- read.csv("climate.csv")
time <- vector()
temp <- vector()

data[data == -9999] <- 0

row = 1

for (year in 1880:2016) {
  
  
  for (month in 1:12) {
    sum = 0
    count = 0
    while (row <= 59223 & month == data[row, 2]) {
      for (col in 4:ncol(data)) {
        sum <- sum + data[row, col]
        count <- count + 1
      }
      row <-row + 1
    }
    value <- sum / count
    time <- c(time, paste(as.character(year), ".", as.character(month), sep = ""))
    temp <- c(temp, value)
  }
}

cleaned_data <- data.frame(t = time,
                 temp = temp)

# data for linear regression
time1 <- vector()
temp1 <- vector()
data[data == -9999] <- 0
row = 1
for (year in 1880:2016) {
  for (month in 1:12) {
    sum = 0
    count = 0
    while (row <= 59223 & month == data[row, 2]) {
      for (col in 4:ncol(data)) {
        sum <- sum + data[row, col]
        count <- count + 1
      }
      row <-row + 1
    }
    value <- sum / count
    time1 <- c(time1, year + (month / 12))
    temp1 <- c(temp1, value)
  }
}
cleaned_data1 <- data.frame(ti = time1,
                            temp = temp1)

linear_model <- lm(cleaned_data1$temp ~ cleaned_data1$ti)

plot_ly() %>%
  add_trace(data = cleaned_data, x = ~t, y = ~temp, 
            mode = "markers",
            text = ~paste("Year: ", t, '\n', "Temperature: ", round(temp, 3)), type = "scatter",
            hoverinfo = "text", 
            marker = list(size = 7, opacity = 0.6)) %>%
  add_trace(data = cleaned_data1, x = ~ti, 
            y = fitted(linear_model), line = list(wideth = 2), type = "scatter", mode = 'lines') %>%
  layout(title = "Average World Temperature(F) from 1880 to 2016")
  










