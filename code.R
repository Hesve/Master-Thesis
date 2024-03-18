#read data


#Reading data


library(haven)
library(readxl)
library(readr)
library(tidyverse)
library(lubridate)
library(forecast)
library(stats)
library(psych)

FED_data <- read.csv("./data/FEDFUNDS.csv")
us_inflation <- read_excel("C:/Users/henry/OneDrive/Year_2/Master Thesis/Main/Code/data/us_inflation.xlsx",
                           skip = 10) # skip the first 10 rows as its just information

riksbanken_data <- read_delim("data/riksbanken_monthly.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

swe_inflation <- read_csv("data/swedish_CPI.csv", 
    col_names = FALSE, skip = 3)

us_unemployment <- read_csv("data/us_UNRATE.csv")
swe_unemployment <- read_csv("data/swe_UNRATE.csv")


#Data cleaning
#Fixing the data frames to remove NA columns as well as  subsetting all data from june 1994 to
# december 2023 #due to Riksbanken data  starting from  from 1994-06

FED_data<- FED_data[-c(1:479, 835),]

us_inflation <- us_inflation %>%  select(-c(14,15)) %>%  pivot_longer( 
                               cols = -Year, 
                               names_to = "Month",  
                               values_to = "InflationRate") %>% slice(-c(1:533))

swe_inflation <- swe_inflation[-c(1:173, 529),] %>%
  rename(Date = X1, InflationRate = X2)

riksbanken_data <- riksbanken_data[-356,]

us_unemployment <- us_unemployment[-c(1:557, 913),]

swe_unemployment <- rbind(swe_unemployment[-c(1:137),], NA) %>% 
  `colnames<-`(c("Date", "UnemploymentRate"))
  
#adding NA to make the data consistent with the rest of the data since decemer 2023 is missing

#combining the data
data <- seq(as.Date("1994-06-01"), length.out = 355, by = "month") %>% 
  format("%Y-%m") %>% #date variable 
  tibble(Date = ., swe_CPI = swe_inflation$InflationRate, 
         us_CPI = us_inflation$InflationRate, us_interest = FED_data$FEDFUNDS, 
         swe_interest = as.numeric(gsub(",", ".", riksbanken_data$Medel)),
         us_unemployment= us_unemployment$UNRATE,
         "swe_unemployment" = swe_unemployment$UnemploymentRate)

psych::describe(data[,-1])

#should probably make this into a nicer wrapper funciton later
par(mfrow=c(3,2))
variables <- colnames(data)[-1] 

for (var in variables){
  select(data, var) %>% 
ts(start = c(1994, 6), frequency = 12) %>% 
    plot()
}

for(var in variables){
  select(data, var) %>% 
    tsdisplay(lag.max = 24, main = var)
}
tsdisplay(data$swe_CPI, lag.max = 24, main = "swe_CPI")


apply(data[,-1], 2, Box.test, lag = 12, type = "Ljung-Box")
#all p <0.05 so we reject the null hypothesis that the data is white noise



# Plot time series
for (col in names(data)[-1]) {
  plot(data[[paste0(col, "_ts")]], main = col, ylab = col)
}
###
#plots
ts.plot(data$us_interest, data$us_CPI)
plot.ts(data$us_interest)


ts(data$swe_CPI, start = c(1994, 6), frequency = 12) %>% plot()

ts.plot(data$swe_interest
        )



riksbanken_data$Medel <- as.numeric(riksbanken_data$Medel)

gsub(",", ".", riksbanken_data$Medel) %>% as.numeric()

#converting to consistent time format


##plots




##




#package for testing the null of flat density
# devtools::install_github("tedwestling/ctsCausal")
# also requries additional packages
# install.packages("sets")
# install.packages("earth")
# install.packages("SuperLearner")

library(ctsCausal)
library(SuperLearner)
library(earth)
library(sets)




#This function performs a hypothesis test that the causal dose-response curve theta(a)
#"is flat on the support of the observed exposure A.
#Y is the outcome, A is the exposure, and W is a matrix of covariates.

#test for swedish data
causalNullTest(Y = data$swe_CPI[-nrow(data)], A=data$swe_interest[-nrow(data)],
               W=data.frame(data$swe_unemployment[-nrow(data)]),
               control = list(cross.fit = FALSE, verbose=TRUE, g.n.bins = 2:5))


#test for us data
causalNullTest(Y = data$us_CPI, A=data$us_interest, W=data.frame(data$us_unemployment), 
               control = list(cross.fit = FALSE, verbose=TRUE), g.n.bins = 2:5))

               
