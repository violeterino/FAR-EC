This repository contains the implementation of  Fuzzy association rules and intermediate quantifiers to real-world macroeconomic data.
The project includes:
  - Downloaded data from FRED and our data (folder original_data)
  - Data transformation (data_transform.R)
  - Transformed data for shiny app (app/data.RDS)
  - A shiny app to present results interactively (app/app.R)
  - survey results (folder survey)

**Data**

This repository includes two time series downloaded from the FRED – Federal Reserve Bank of St. Louis website:

**Consumer Price Index: Total All Items for the United States (original_data/CPALTT01USM659N.csv)**

Series ID: CPALTT01USM659N

Period: February 1997 – December 2023

Source: FRED

Citation:

Organization for Economic Co-operation and Development, Consumer Price Indices (CPIs, HICPs), COICOP 1999: Consumer Price Index: Total for United States [CPALTT01USM659N], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/CPALTT01USM659N, July 10, 2025. 

**One-Year Inflation Expectations (original_data/EXPINF1YR.csv)**

Series ID: EXPINF1YR

Period: February 2000 - January 2024

Source: FRED

Citation:

Federal Reserve Bank of Cleveland, 1-Year Expected Inflation [EXPINF1YR], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/EXPINF1YR, July 10, 2025. 

This repository also includes time series of tone - a qualitative proxy for the sentiment of CB messages (original_data/data.xlsx)

**Data processing**

The script data_transform.R performs the following tasks:

- loads the .csv and .xlsx data files
- compute middle points of context for evolving context
- saves the output in .rds format.

This .rds file is then consumed by the Shiny app located in the app directory.

**Shiny application**

The Shiny application (app/app.R) enables interactive exploration of the method's results. It is available online here:
🔗 [https://fiala.shinyapps.io/fedapp/]
