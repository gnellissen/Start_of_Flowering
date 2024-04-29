weather_data <- function(){

## Libraries ##
require(httr)
require(jsonlite)
require(ncdf4)
require(raster)
require(dplyr)
require(lubridate)

## Retrieve climate data ##
# We use the complete historical data for weather station De Bilt
# Url for data De Bilt. 260 = De Bilt.
url = "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/etmgeg_260.zip"
# Prepare temp file 
temp = tempfile()
# Only download the file once
if (exists(".weather_data") == FALSE){
# download data from url to tempfile
download.file(url, temp)
# unzip tempfile and read the .txt file within. Skip first 49 lines with headers
.weather_data <<- read.csv(unz(temp, "etmgeg_260.txt"), sep = ",", skip = 52, header = F)
}

historical_data <- .weather_data
# set column names for the data
names(historical_data) = c("STN",        # Station
                           "YYYYMMDD",   # Date (YYYY=year MM=month DD=day)
                           "DDVEC",      # Vector mean wind direction in degrees (360=north, 90=east, 180=south, 270=west, 0=calm/variable)
                           "FHVEC",      # Vector mean windspeed (in 0.1 m/s)
                           "FG",         # Daily mean windspeed (in 0.1 m/s) 
                           "FHX",        # Maximum hourly mean windspeed (in 0.1 m/s)
                           "FHXH",       # Hourly division in which FHX was measured
                           "FHN",        # Minimum hourly mean windspeed (in 0.1 m/s)
                           "FHNH",       # Hourly division in which FHN was measured
                           "FXX",        # Maximum wind gust (in 0.1 m/s)
                           "FXXH",       # Hourly division in which FXX was measured
                           "TG",         # Daily mean temperature in (0.1 degrees Celsius)
                           "TN",         # Minimum temperature (in 0.1 degrees Celsius)
                           "TNH",        # Hourly division in which TN was measured
                           "TX",         # Maximum temperature (in 0.1 degrees Celsius)
                           "TXH",        # Hourly division in which TX was measured
                           "T10N",       # Minimum temperature at 10 cm above surface (in 0.1 degrees Celsius)
                           "T10NH",      # 6-hourly division in which T10N was measured; 6=0-6 UT, 12=6-12 UT, 18=12-18 UT, 24=18-24 UT 
                           "SQ",         # Sunshine duration (in 0.1 hour) calculated from global radiation (-1 for <0.05 hour)
                           "SP",         # Percentage of maximum potential sunshine duration
                           "Q",          # Global radiation (in J/cm2)
                           "DR",         # Precipitation duration (in 0.1 hour)
                           "RH",         # Daily precipitation amount (in 0.1 mm) (-1 for <0.05 mm)
                           "RHX",        # Maximum hourly precipitation amount (in 0.1 mm) (-1 for <0.05 mm)
                           "RHXH",       # Hourly division in which RHX was measured
                           "PG",         # Daily mean sea level pressure (in 0.1 hPa) calculated from 24 hourly values
                           "PX",         # Maximum hourly sea level pressure (in 0.1 hPa)
                           "PXH",        # Hourly division in which PX was measured
                           "PN",         # Minimum hourly sea level pressure (in 0.1 hPa)
                           "PNH",        # Hourly division in which PN was measured
                           "VVN",        # Minimum visibility; 0: <100 m, 1:100-200 m, 2:200-300 m,..., 49:4900-5000 m, 50:5-6 km, 56:6-7 km, 57:7-8 km,..., 79:29-30 km, 80:30-35 km, 81:35-40 km,..., 89: >70 km)
                           "VVNH",       # Hourly division in which VVN was measured
                           "VVX",        # Maximum visibility; 0: <100 m, 1:100-200 m, 2:200-300 m,..., 49:4900-5000 m, 50:5-6 km, 56:6-7 km, 57:7-8 km,..., 79:29-30 km, 80:30-35 km, 81:35-40 km,..., 89: >70 km)
                           "VVXH",       # Hourly division in which VVX was measured
                           "NG",         # Mean daily cloud cover (in octants, 9=sky invisible)
                           "UG",         # Daily mean relative atmospheric humidity (in percents)
                           "UX",         # Maximum relative atmospheric humidity (in percents)
                           "UXH",        # Hourly division in which UX was measured
                           "UN",         # Minimum relative atmospheric humidity (in percents)
                           "UNH",        # Hourly division in which UN was measured
                           "EV24")       # Potential evapotranspiration (Makkink) (in 0.1 mm)
# remove tempfile
unlink(temp)
# add some extra columns to the data with year, month, day and day of year
# year, month and day are taken from YYYYMMDD
# DOY is calculated by changing YYYYMMDD to a date datatype and then formatting it as day only
# %j: Day of year as decimal number (001-366): For input, 366 is only valid in a leap year.
historical_data$year = as.numeric(substr(historical_data$YYYYMMDD, 1, 4))
historical_data$month = as.numeric(substr(historical_data$YYYYMMDD, 5, 6))
historical_data$day = as.numeric(substr(historical_data$YYYYMMDD, 7, 8))
historical_data$DOY = as.numeric(strftime(as.Date(as.character(historical_data$YYYYMMDD), "%Y%m%d"), format = "%j"))
# daily precipitation < 0.05mm is given as -1. We'll set it to 0 here
historical_data$RH[historical_data$RH < 0] = 0

## Get long-year average for every DOY ##
# create a dataframe for the future data with DOY
future_data = data.frame(1:366)
colnames(future_data) = "DOY"
# function for calculating the mean of a specific variable of a certain DOY over the last 15 years
# x is the DOY to calculate the mean for, variable is the column name of the variable to calculate the mean for
# data is retrieved from the dataframe historical_data
get_mean_variable = function(x, variable = "TG") mean(historical_data[,variable][historical_data$DOY == x & historical_data$year >= max(historical_data$year) - 15 & historical_data$year != max(historical_data$year)])
# in future data dataframe, set columns for daily mean temperature, daily max temperature, global radiation and daily precipitation. The mean for each DOY is taken from the last 15 years of historical data
future_data$TG15Y = sapply(future_data$DOY, get_mean_variable)
future_data$TX15Y = sapply(future_data$DOY, get_mean_variable, variable = "TX")
future_data$Q15Y = sapply(future_data$DOY, get_mean_variable, variable = "Q")
future_data$RH15Y = sapply(future_data$DOY, get_mean_variable, variable = "RH")

## Get forecast data ##
# get weather forecast from meteoserver
# pc=3731GA is the postal code, key=d17f206fbb is the API key
endpoint = "https://data.meteoserver.nl/api/uurverwachting_gfs.php?pc=3731GA&key=d17f206fbb"
# GET() is a function from httr package to retrieve information identified by the request URI. It returns a response() object
raw_data = GET(endpoint)
# retrieve content from the raw_data, get the raw characters (from hex?) and read the data from the resulting json
prediction_data = fromJSON(rawToChar(raw_data$content))$data
# set data to numeric
prediction_data$temp = as.numeric(prediction_data$temp)             # temp: temperature in degrees Celsius
# windrltr: wind direction
prediction_data$gust = as.numeric(prediction_data$gust)             # gust: windgust speed in m/s
prediction_data$gustb = as.numeric(prediction_data$gustb)           # gustb: windgust speed in Beaufort
prediction_data$gustkt = as.numeric(prediction_data$gustkt)         # gustkt: windgust speed in knots
prediction_data$gustkmh = as.numeric(prediction_data$gustkmh)       # gustkmh: windgust speed in km/h
prediction_data$vis = as.numeric(prediction_data$vis)               # vis: vision in meters
prediction_data$neersl = as.numeric(prediction_data$neersl)         # neersl: precipitation in mm
prediction_data$rv = as.numeric(prediction_data$rv)                 # rv: humidity in %
prediction_data$gr = as.numeric(prediction_data$gr)                 # gr: global radiation in W/m2
prediction_data$tw = as.numeric(prediction_data$tw)                 # tw: total cloud cover in %
prediction_data$cape = as.numeric(prediction_data$cape)             # cape: collective available potential energy (thunderstorm energy) in J/kg
# set new columns for year, month, day and date from tijdnl
prediction_data$year = as.numeric(substr(prediction_data$tijd_nl, 7, 10))
prediction_data$month = as.numeric(substr(prediction_data$tijd_nl, 4, 5))
prediction_data$day = as.numeric(substr(prediction_data$tijd_nl, 1, 2))
prediction_data$date = as.Date(prediction_data$tijd_nl, "%d-%m-%Y")
# since the prediction data comes per hour or per 3 hours (dependent on how far in the future it is), create a dataframe for one value per day
prediction = data.frame(as.character(unique(as.Date(prediction_data$tijd_nl, "%d-%m-%Y"))))
colnames(prediction) = "date"
# function for calculating the mean value of a variable on a certain date
# x is the date, variable is the variable to calculate the mean for
# data is retrieved from the prediction_data dataframe
get_mean_variable_predict = function(x, variable = "temp") mean(prediction_data[,variable][as.character(as.Date(prediction_data$tijd_nl, "%d-%m-%Y")) == x])
# function for calculating the max value of a variable on a certain date 
get_max_variable_predict = function(x, variable = "temp") max(prediction_data[,variable][as.character(as.Date(prediction_data$tijd_nl, "%d-%m-%Y")) == x])
# get mean temperature, solar radiation and precipitation for each day of the prediction
prediction$TG = sapply(prediction$date, get_mean_variable_predict) * 10
prediction$Q = sapply(prediction$date, get_mean_variable_predict, variable = "gr") * 0.0001 * 60 * 60 * 24 * 2 # from W/m2 to J/cm2/day and * 2 because it is wrong...
prediction$RH = sapply(prediction$date, get_mean_variable_predict, variable = "neersl") * 10 * 24
# get maximum temperature for each day of the prediction
prediction$TX = sapply(prediction$date, get_max_variable_predict) * 10
# remove the last day of the prediction because it's often an incomplete day
prediction = head(prediction, -1)
# also set the DOY, calculated the same way as before
prediction$DOY = as.numeric(strftime(as.Date(prediction$date), format = "%j"))
# Include year column
prediction$year <- year(prediction$date)

# calculate cumalitive values per year for mean daily temperature, max daily temperature, global radiation and precipitation
# temperatures below 0 are set to 0
historical_data$cumTG = ave(pmax(0, historical_data$TG), historical_data$year, FUN =  cumsum)
historical_data$cumTX = ave(pmax(0, historical_data$TX), historical_data$year, FUN =  cumsum)

################################################################################
## END
################################################################################

# Add cum sum to average weather for past 15 years
future_data$cumTG15Y <- round(cumsum(future_data$TG15Y), digits = 0)
future_data$cumQ15Y <- round(cumsum(future_data$Q15Y), digits = 0)
future_data$cumRH15Y <- round(cumsum(future_data$RH15Y), digits = 0)

# Simplify historical_data
historical_data %>%
  filter(YYYYMMDD > 19600000) %>% # Select only records after 1960
  mutate(date = as.Date(as.character(YYYYMMDD), "%Y%m%d")) %>%
  .[,c("date","year","TG","Q","RH","TX","DOY")] -> historical_data # Select only most important columns

# Combine data frames
result <- rbind(historical_data, prediction)

result %>%
  mutate(cumTG = ave(pmax(0, result$TG), result$year, FUN =  cumsum)) %>%  # Add cumulative temperature sum
  mutate(cumQ = ave(pmax(0, result$Q), result$year, FUN =  cumsum)) %>% # Add cumulative radiation sum
  mutate(cumRH = ave(pmax(0, result$RH), result$year, FUN = cumsum)) %>% # Add cumulative precipitation sum
  mutate(across(c(TG,Q,RH,cumTG, cumQ, cumRH), ~round(.,digits = 0))) %>% # Make sure everything is rounded
  mutate(cumTGdiff = cumTG - future_data[match(DOY, future_data$DOY), "cumTG15Y"]) %>% 
  mutate(cumQdiff = cumQ - future_data[match(DOY, future_data$DOY), "cumQ15Y"]) %>% 
  mutate(cumRHdiff = cumRH - future_data[match(DOY, future_data$DOY), "cumRH15Y"])-> result


return(result)
}



