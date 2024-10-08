#' Retrieve and process meteorological data from weather stations.
#'
#' @param lat Numeric. Latitude of the location.
#' @param lon Numeric. Longitude of the location.
#' @param interval Character. Time interval for the data ("day", "hour"). Defaults to "day".
#' @param start Character. Start date in "YYYY-MM-DD" format. Defaults to "2018-02-01".
#' @param end Character. End date in "YYYY-MM-DD" format. Defaults to "2018-04-15".
#'
#' @return Data frame with weather data, predicted missing values filled using random forest.
#' @importFrom dplyr select slice mutate group_by ungroup row_number select_if ends_with filter
#' @importFrom weathercan stations_meta stations_dl stations_search weather_dl
#' @importFrom janitor remove_empty
#' @importFrom lubridate today year
#' @importFrom randomForest randomForest
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage of meteocan
#' lat <- 45.4215   # Latitude for Ottawa, Canada
#' lon <- -75.6972  # Longitude for Ottawa, Canada
#'
#' # Retrieve weather data for Ottawa from February 1, 2018, to April 15, 2018
#' weather_data <- meteocan(lat, lon, interval = "day", start = "2018-02-01", end = "2018-04-15")
#'
#' # View the first few rows of the data
#' head(weather_data)
#' }
meteocan <- function(lat, lon, interval = "day", start = "2018-02-01", end = "2018-04-15") {

  # Check if weather station metadata needs updating
  date <- weathercan::stations_meta()
  if (as.numeric(lubridate::today() - date$weathercan_modified) > 90) {
    message("The weather station database is outdated. Updating...")
    weathercan::stations_dl()
  }

  # Find the 5 closest weather stations to the given coordinates
  station <- weathercan::stations_search(coords = c(lat, lon), dist = 200, interval = interval,
                                         starts_latest = lubridate::year(start),
                                         ends_earliest = lubridate::year(end),
                                         quiet = T) |>
    dplyr::slice(1:5)

  # Download weather data from the identified stations
  w <- weathercan::weather_dl(station_ids = station$station_id, start = start, end = end, interval = interval,
                              quiet = T)

  # Clean and prepare data
  w <- janitor::remove_empty(w, which = "cols") |>
    dplyr::select(-dplyr::ends_with("_flag")) |>
    dplyr::group_by(station_id) |>
    dplyr::mutate(time_id = dplyr::row_number()) |>
    dplyr::ungroup()

  # Select numeric columns for weather data
  weather <- w |>
    dplyr::select_if(is.numeric) |>
    dplyr::select(-lat, -lon, -elev)

  # Fill missing values with Random Forest predictions for numeric variables
  for (i in colnames(weather |> dplyr::select(-station_id, -time_id))) {

    if (length(unique(weather[[i]])) >= 5) {
      # Create model for each variable if enough unique values are present
      weather$variable <- weather[[i]]

      # Random forest model to predict missing values
      rf <- randomForest::randomForest(variable ~ station_id + time_id, data = weather,
                                       na.action = na.omit, mtry = 2, ntree = 5000)

      # Predict and replace missing values
      weather <- weather |>
        dplyr::mutate(pred = predict(rf, weather |> dplyr::select(station_id, time_id))) |>
        dplyr::mutate(variable = ifelse(is.na(variable), pred, variable))

      # Assign the imputed variable back to the column
      weather[[i]] <- weather$variable
    }
  }

  # Join the numeric data back to the original dataset
  ww <- w |>
    dplyr::filter(station_id == station$station_id[1]) |>
    dplyr::select(-dplyr::where(is.numeric), station_id, time_id, lat, lon, elev) |>
    dplyr::left_join(weather)

  return(ww)
}
