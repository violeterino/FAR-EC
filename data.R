library(tidyverse)
library(rmake)
library(readxl)


input <- getParam('.depends')
output <- getParam('.target')


raw_fed <- read_xlsx(input[1])



raw_all_1 <- raw_fed %>%
    select(year,month,tone_BN,inf_exp,inf)




saveRDS(raw_fed, output[1])

