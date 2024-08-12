#Note: requires the folder "data" in the directory

library(haven)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)

FED_data <- read.csv("./data/FEDFUNDS.csv")

#us_inflation <- read_excel("./data/us_inflation.xlsx",
                          # skip = 10) # skip the first 10 rows as its just information

us_inflation <- read_csv("./data/us_cpi_12_month.csv",skip=0)

us_PCE <- read_csv("./data/us_PCE.csv",skip=c(3))
us_PCE <- t(us_PCE[2,])#removing reduant variables and information
us_PCE <- data.frame("PCE"= us_PCE[-c(1,2)], "Year" = rownames(us_PCE)[-c(1,2)])
us_PCE$PCE <- as.numeric(us_PCE$PCE)

swe_KPIF <- read_excel("./data/swe_kpif.xls", skip = 6, sheet="Data")

riksbanken_data <- read_delim("data/riksbanken_monthly.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

swe_inflation <- read_csv("./data/swedish_CPI.csv", 
                          col_names = FALSE, skip = 3)


us_unemployment <- read_csv("./data/us_unrate_SA.csv")
swe_unemployment <- read_csv("./data/swe_unrate_SA.csv")


#Data cleaning
#Fixing the data frames to remove NA columns as well as  subsetting all data from june 1994 to
# december 2023 #due to Riksbanken data  starting from  from 1994-06

FED_data<- FED_data[-c(1:which(FED_data$DATE=="1994-05-01"), 835),]




us_inflation <-us_inflation[which(us_inflation$Label=="1994 Jun"):nrow(us_inflation),
                            c(4,6)]
colnames(us_inflation) <-  c("Date", "InflationRate")
us_inflation$InflationRate <- as.numeric(us_inflation$InflationRate)


swe_inflation <- swe_inflation[-c(1:173, 529),] 
colnames(swe_inflation) <- c("Date", "InflationRate")

swe_KPIF <- swe_KPIF[54:408,c(1,5)]
  

riksbanken_data <- riksbanken_data[-nrow(riksbanken_data),]

us_unemployment <- us_unemployment[-c(1:which(us_unemployment$DATE =="1994-05-01")),]

swe_unemployment <- swe_unemployment[-c(1:which(swe_unemployment$DATE=="1994-05-01")),]
colnames(swe_unemployment) <- c("Date", "UnemploymentRate")

us_PCE <- us_PCE[414:768,]

dates <- seq(as.Date("1994-06-01"), length.out = 355, by = "month") 


# Format the dates to "YYYY-MM"
dates <- format(dates, "%Y-%m")

#Merging all the data into single tibble
data <- tibble(
  Date = dates,
  swe_CPI = swe_inflation$InflationRate,
  us_CPI = us_inflation$InflationRate,
  us_interest = FED_data$FEDFUNDS,
  swe_interest = as.numeric(gsub(",", ".", riksbanken_data$Medel)),
  us_unemployment = us_unemployment$UNRATE,
  swe_unemployment = swe_unemployment$UnemploymentRate,
  swe_KPIF = swe_KPIF$KPIF,
  us_PCE = us_PCE$PCE
)

save(data, file = "./data.RData")


#quartlery data
dates_new <- mutate(data, Date = as.Date(paste0(Date, "-01"))) 

data_quarterly <-  mutate(dates_new, Year = year(Date),
         Quarter = quarter(Date))

library(lubridate)

# Group by Year and Quarter and calculate the average for each variable
data_quarterly <-data_quarterly %>%
  group_by(Year, Quarter) %>%
  summarise(across(c(swe_CPI, us_CPI, us_interest, swe_interest, us_unemployment, swe_unemployment, swe_KPIF, us_PCE), mean, na.rm = TRUE)) %>%
  ungroup()

data_quarterly <- apply(data_quarterly[,-1],2, FUN= function(x) round(x,2))
save(data_quarterly, file = "./data_quarterly.RData")



######
#old code may be removed later
##

#us_inflation <- us_inflation %>%  select(-c(14,15)) %>%  pivot_longer( 
# cols = -Year, 
#names_to = "Month",  
#values_to = "InflationRate") %>% 
#filter(row_number() >= which(Year == 1994 & Month == "Jun"))

#combining the data
#data <- seq(as.Date("1994-06-01"), length.out = 355, by = "month") %>% 
#format("%Y-%m") %>% #date variable 
# tibble(Date = ., swe_CPI = swe_inflation$InflationRate, 
#  us_CPI = us_inflation$InflationRate, us_interest = FED_data$FEDFUNDS, 
#  swe_interest = as.numeric(gsub(",", ".", riksbanken_data$Medel)),
#  us_unemployment= us_unemployment$UNRATE,
#  "swe_unemployment" = swe_unemployment$UnemploymentRate)

