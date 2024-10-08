# meteocan

**meteocan** is an R package designed to retrieve and process meteorological data from nearby weather stations, with missing data imputed using random forests.

## Features
- Automatically retrieves weather station metadata and updates it if outdated.
- Finds the 5 closest weather stations to a given location (latitude, longitude).
- Downloads weather data for specified time intervals (day, hour).
- Cleans and processes the weather data, filling in missing values with Random Forest predictions.
- Supports time intervals from daily to hourly data.

## Installation

To install the development version of **meteocan** from GitHub, you will need the `devtools` package:

```r
# Install devtools if you don't have it
install.packages("devtools")

# Install meteocan from GitHub
devtools::install_github("cedricbouffard/meteocan")
```
