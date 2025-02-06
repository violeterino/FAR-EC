library(tidyverse)
library(rmake)
library(readxl)
library(forecast)

input <- getParam('.depends')
output <- getParam('.target')

#Import
FEDdata <- readRDS(input[1])
evolving <- read_xlsx(input[2])

#Selection of relevant months
evolving <- evolving %>% mutate(date = ymd(date)) %>%
    filter(date > "1997-01-31", date < "2024-01-01")
#variable for loop
borderline <- nrow(FEDdata)

#Models
inf_ma1 <- NULL
inf_ar1 <- NULL
inf_arma11 <- NULL
inf_auto <- NULL
inf_naive <- NULL

for (i in 1:borderline){
    data_lapse <- evolving[c(i:(i+35)),]
    inf_exp <- as.ts(data_lapse[["inf"]])
    MA1 <- arima(inf_exp, order = c(0,0,1))
    AR1 <- arima(inf_exp, order = c(1,0,0), method = "ML")
    ARMA11 <- arima(inf_exp, order = c(1,0,1), method = "ML")
    AUTO <- auto.arima(inf_exp)



    inf_ma1[i] <- predict(MA1)$pred[1]
    inf_ar1[i] <- predict(AR1)$pred[1]
    inf_arma11[i] <- predict(ARMA11)$pred[1]
    inf_auto[i] <- forecast(AUTO)$mean[1]


}

evolving2 <- evolving %>% mutate(date = ymd(date)) %>%
    filter(date >= "2000-01-01", date < "2024-01-01")

for (i in 1:borderline){
   inf_naive[i] <- evolving2[["inf"]][i]
}


FEDdata <- data.frame(FEDdata,
                      inf_ma1 = inf_ma1,
                      inf_ar1 = inf_ar1,
                      inf_arma11 = inf_arma11,
                      inf_auto = inf_auto,
                      inf_naive = inf_naive)
FEDdata <- FEDdata %>% select(
    year,month,tone_BN,inf_exp,inf,inf_ma1,inf_ar1,
    inf_arma11, inf_auto, inf_naive
)

saveRDS(FEDdata, output[1])
