#Packages
library(tidyverse)
library(forecast)
library(readxl)

#Import
data <- read_xlsx("original_data/data.xlsx")
inf_expectations <- read_csv("original_data/EXPINF1YR.csv")
inflation <- read_csv("original_data/CPALTT01USM659N.csv")



#Selection of relevant months
evolving1 <- inflation %>% mutate(observation_date = mdy(observation_date)) %>%
    dplyr::filter(observation_date > "1997-01-31", observation_date < "2024-01-01")
evolving2 <- inflation %>% mutate(observation_date = mdy(observation_date)) %>%
    dplyr::filter(observation_date >= "2000-01-01", observation_date < "2024-01-01")
#variable for loop
borderline <- nrow(data)

#Models
inf_ma1 <- NULL
inf_ar1 <- NULL
inf_arma11 <- NULL
inf_auto <- NULL
inf_naive <- NULL

for (i in 1:borderline){
    data_lapse <- evolving1[c(i:(i+35)),]
    inf_exp <- as.ts(data_lapse[["CPALTT01USM659N"]])
    MA1 <- arima(inf_exp, order = c(0,0,1))
    AR1 <- arima(inf_exp, order = c(1,0,0), method = "ML")
    ARMA11 <- arima(inf_exp, order = c(1,0,1), method = "ML")
    AUTO <- auto.arima(inf_exp)

    inf_ma1[i] <- predict(MA1)$pred[1]
    inf_ar1[i] <- predict(AR1)$pred[1]
    inf_arma11[i] <- predict(ARMA11)$pred[1]
    inf_auto[i] <- forecast(AUTO)$mean[1]
}

for (i in 1:borderline){
   inf_naive[i] <- evolving2[["CPALTT01USM659N"]][i]
}

#Final data.frame
data <- data.frame(data,
                   inf_exp = inf_expectations$EXPINF1YR,
                   inf_ma1 = inf_ma1,
                   inf_ar1 = inf_ar1,
                   inf_arma11 = inf_arma11,
                   inf_auto = inf_auto,
                   inf_naive = inf_naive)
#Export data.frame
saveRDS(data, "app/data.RDS")
