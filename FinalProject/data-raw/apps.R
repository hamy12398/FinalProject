## code to prepare `apps` dataset goes here
library(readr)
data.raw <- read_csv("data-raw/googleplaystore.csv",
                     col_types = cols(Installs = col_number()))

options(scipen = 999)

######################## Clean Data ########################
attach(data.raw)
library(stringr)
library(lubridate)
####### Transform data
## Size (Kb)
data = data.raw
data$Size[grep('M$', data.raw$Size)] = as.numeric(str_remove(
  data$Size[grep('M$', data.raw$Size)] ,"[M]"))
data$Size[grep('k$', data.raw$Size)] = as.numeric(str_remove
                                                  (data$Size[grep('k$', data.raw$Size)] ,"[k]"))
data$Size[grep('M$', data.raw$Size)] = as.numeric(data$Size[grep('M$', data.raw$Size)])*(1024)
data$Size <- as.numeric(data$Size)

data <- data[!is.na(data$Size),]
## Rating
data$Rating <- as.numeric(data$Rating)
## Price
data$Price = as.numeric(str_remove(data$Price[grep('$', data$Price)],"[$]"))
## Genre
length(data$Genres[grep('Action', data$Genres)])
## Last updated
data$`Last Updated` = mdy(data$`Last Updated`)
### NA removing
data <- data[!is.na(data$Size),]
data <- data[-grep('NaN',data$Rating),]
data <- data[!is.na(data$Rating),]
data <- data[!is.na(data$Price),]

apps <- data
usethis::use_data(apps, overwrite = TRUE)
