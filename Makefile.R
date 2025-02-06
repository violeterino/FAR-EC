library(rmake)

job <- c(
    c("incoming/FEDdata.xlsx") %>>%
             rRule('data.R') %>>% c("data/FEDdata.rds"),
    c("data/FEDdata.RDS","incoming/data_for_evolving.xlsx") %>>%
        rRule("evolving.R") %>>% ("fedapp/FEDdata.RDS")
   )
makefile(job, "Makefile")
