# Forecasting Parking Meter Transactions in San Diego
Author: Stephen Reagin
Contact: sreagin@sandiego.edu

This is a repository for a time series project fulfilling the requirements ADS 506 - Applied Time Series Analysis

---
## Introduction

Hello, world! Thank you for visiting. 

The intent of this project is to build time series models that can reliably and accurately forecast parking meter transactions volume in San Diego over a 90-day and 180-day forecast horizon. This will help inform decisions related to parking meter enforcement staffing, as described in the project presentation as well as the written report.

The purpose of this repository is to provide the data and analysis files in Python and R so that you, the curious reader,
can audit and/or recreate the analysis and time series modeling yourself.

You can read more about the Motivation, Methods, and Results in the [Final Report](https://github.com/sfreagin/parking_meters/blob/main/final_report/FinalReport.pdf), which also provides a blueprint for recreating the analysis.

## Contents of the Repository

This repository is organized under the following subdirectories (only important files are listed):

* **datasets/** - this contains both curated datasets and raw data files:
  * Processed datasets from the EDA notebook
    * `daily_total_transactions.csv` - main dataset for analysis
    * `daily_total_amounts.csv` - unused dataset for analysis
    * `daily_amount_by_pole.csv` - unused dataset for building interactive Tableau dashboard
    * `daily_transactions_by_pole.csv` - unused dataset for building interactive Tableau dashboard
  * Raw Parking files originally sourced from the [City of San Diego Open Data Portal](https://data.sandiego.gov/datasets/parking-meters-transactions/):
    * `treas_meters_20xx_by_mo_pole_id.csv` - raw parking meter data for every year 2018 - 2023
  * Raw weather data from the [NOAA](https://www.ncdc.noaa.gov/cdo-web/webservices/v2) 
    * `sd_2018_2023.csv` - daily temperature and precipitation information 2018 - 2023
* **final_report/** contains the written academic report
  * `FinalReport.pdf` is the important file, everything else includes a PDF of the Python and/or R code
* **notebooks/** - contains Jupyter notebook files for Python and visual preprocessing
  * `EDA.ipynb` - main Exploratory Data Analysis file
  * `noaa_api.ipynb` - calls the NOAA API (with appropriate API key) to pull bulk daily weather information
* **R_files/** - times series models and scratch notes for coding in `R`
  * `finalreport_markdown.rmd` is the main file for generating time series models and forecasts, along with visuals
  * `finalreport_markdown.html` is the same file but in HTML form (can be read in a browser)
* **deprecated_files/** - you can ignore these :)
 
The main repository also has two files related to an audiovisual presentation describing the business case for this research:
* `StephenReagin_parkingMeters_video.mp4` - a 9 minute slideshow presentation
* `StephenReagin_parkingMeters.pdf` - the corresponding slide deck

### Data Sources

Parking information is originally sourced from the [City of San Diego Open Data Portal](https://data.sandiego.gov/datasets/parking-meters-transactions/), which is a wonderful resource for accessing free municipal data sources. Weather information comes from the [National Oceanic & Atmospheric Administration (NOAA)](https://www.ncdc.noaa.gov/cdo-web/webservices/v2) and their free Climate Data Online API.

### Results

It is possible to build time series models which reliably forecast parking meter transactions over a 90-day period within +/-5% of the actual values and having RMSE less than 1685 or half a standard deviation (of the transactions volume arithmetic mean). 

The best results were obtained by removing Sundays and Holidays from the training dataset and applying seasonal decomposition methods (STL) before feeding into ETS and ARIMA models. 

Linear Regression also yielded terrific forecasts after removing Sundays and Holidays, but it relies on knowledge of weather data which can only be predicted 7-10 days into the future, so it will be better served as a backwards-looking model to see if there are anomalies.

## For more information

I highly recommend reviewing the online textbook [Forecasting: Principles and Practice](https://otexts.com/fpp3/) which very clearly outlines a number of times series methods, including their underlying philosophical assumptions and mathematical frameworks, and provides detailed `R` code to implement and integrate multiple time series models in the `R` environment.

`Hyndman, R.J., & Athanasopoulos, G. (2021) Forecasting: principles and practice, 3rd edition, OTexts: Melbourne, Australia. https://otexts.com/fpp3/`

It's also worthwhile to review the `fable` library and its [reference documentation](https://fable.tidyverts.org/reference/index.html) for time series models. In particular, I found these reference pages incredibly helpful:
* Estimate an ARIMA model https://fable.tidyverts.org/reference/ARIMA.html
* Exponential smoothing state space model (i.e. an ETS model ) https://fable.tidyverts.org/reference/ETS.html
