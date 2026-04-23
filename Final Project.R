#Jamie Boyd
#Final Data Project

#Finding right dataset

Consumption = read.csv("/cloud/project/finalproject/use_NY.csv")
#Names = read.xlsx("/cloud/project/finalproject/Names.xlsx")

#tidyr
#pivot_longer

install.packages(c("ggplot2", "dplyr", "olsrr", "PerformanceAnalytics", "lubridate", "forecast" ))

library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)
library(lubridate)
library(forecast)
library(tidyr)


#Cleaning Data
names(Consumption)

Consumption = Consumption %>%
  select(-Data_Status, -State)

names(Consumption) <- sub("X", "", names(Consumption))

consumption_long <- pivot_longer(Consumption,cols = -MSN, names_to = "year",
  values_to = "consumption")




