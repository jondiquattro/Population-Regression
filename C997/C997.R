install.packages("readxl")
install.packages('xlsx')
install.packages("modelr")
install.packages("tidyverse")
library(xlsx)
library(modelr)
library(tidyverse)
require(stats)
library(readxl)
setwd("~/R-Labs/C997")

#read data
state_data <- read_excel("StateData.xlsx")

#access nc via deployer didn't work great results in another tibble
NC <- state_data %>% filter(1 == 'North Carolina')

#access better than filter
nc <-state_data %>% slice(42)


#convert vector 4 from char to int
nc[4] <- as.double(nc[4])

#convert to double
nc_years <- c(nc[4:12])
nc_years <- lapply(nc_years, as.double)
nc_vector <- as.vector(unlist(nc_years, use.names=FALSE))


#*-------------plot-----------------//

Year <- 2010:2018
New_Years <- 2019:2025
Population <- nc_vector

#predict

model <- lm(Population~Year)
prediction <- predict(model, newdata=data.frame(Year=New_Years))

#Update
Year <- c(Year, New_Years)
Population <- c(Population, prediction)

plot(Year, Population, ylim=c(min(Population), max(Population)), type="o", pch=16,
     ylab="Population Growth")
abline(model, col="red")
summary(model)
