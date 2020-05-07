library(stringr)
library(purrr)
library(magrittr)
library(data.table)
library(fst)
library(ggplot2)
library(glue)

# countryFltr <- c("DEU", "GBR", "FRA", "ESP", "ITA", "NDL", "BEL", "SWE", "CHE", "AUT")
countryFltr <- "CHN"

#--------------#
# Read in Data #
#--------------#

# Satellite Temp Data
tempData <-
  rbindlist(
    map(
      list.files("dataframes/temps", full.names = TRUE),
      function(x){
        read.fst(x, as.data.table = TRUE)[,date := NULL]
      }
    )
  )[
    country %in% countryFltr
  ][
    ,
    date := as.Date(date_time)
  ][
    ,
   .(temp = mean(temp)),
   by = .(date)
  ]

maxDate <- max(tempData$date)
tempTs <- ts(tempData$temp, end = c(lubridate::year(maxDate),
                                    lubridate::yday(maxDate)), 
             frequency = 365)

# Forecast missing
seasonalModel <- stl(tempTs, s.window = "period")

tempForecast <- 
tsbox::ts_data.table(forecast::forecast(seasonalModel, h = 60)$mean)[
  ,
  `:=`(
    date = as.Date(time)
  )
][,.(date, temp = value)]

tempData <- 
rbind(tempData, tempForecast)[
  order(date),
  roll_temp := frollapply(temp, n = 14, FUN = mean, fill = NA, align = "right")
  ][!is.na(roll_temp)]

# Ingest Satellite Data
satData <-
  rbindlist(
    map(
      list.files("dataframes/all", full.names = TRUE),
      function(x){

        date <- x %>% str_remove("dataframes/all/") %>% str_remove(".fst") %>% as.Date()

        read.fst(x, as.data.table = TRUE)[
          country %in% countryFltr,
          .(cty_avg = mean(no2tc))
          ][
            ,
            `:=`(
              date = date,
              year = lubridate::year(date) %>% as.factor(),
              day  = lubridate::yday(date)
            )
          ]
      }
    )
  )[,
    .(cty_avg = mean(cty_avg)),
    by = date
  ][# Order by date and roll average country no2 emissions
    order(date),
    roll_avg := frollapply(cty_avg, n = 14, FUN = mean, fill = NA, align = "right")
  ][!is.na(roll_avg)]

# Load Weekly Economic Index Data
weiData <-
  fread("dataframes/WEI.csv")[
    ,
    .(date = as.Date(DATE), wei = WEI)
  ]

# Load S&P 500 data
spData <-
  fread("dataframes/SP500.csv")[
    ,
    .(date = as.Date(DATE), stocks = as.numeric(SP500))
  ]

#----------------------#
# Start Data Analytics #
#----------------------#

# Merge data
fullData <-
  tempData %>%
  merge(satData, by = "date", all = TRUE) %>%
  merge(weiData, by = "date", all.x = TRUE) %>%
  merge(spData, by = "date", all.x = TRUE)

fullData <-
  fullData[
    order(date),
    `:=`(
      wei = zoo::na.approx(wei, na.rm = FALSE),
      stocks = zoo::na.approx(stocks, na.rm = FALSE),
      day = lubridate::yday(date),
      year = lubridate::year(date) %>% as.factor(),
      returns = stocks / shift(stocks, 1, fill = NA) - 1 ,
      month = lubridate::month(date) %>% as.factor(),
      quarter = lubridate::quarter(date) %>% as.factor()
    )
  ][
    ,
    `:=`(
      temp      = zoo::na.locf(temp, na.rm = FALSE),
      roll_temp = zoo::na.locf(roll_temp, na.rm = FALSE),
      wei       = zoo::na.locf(wei, na.rm = FALSE),
      stocks    = zoo::na.locf(stocks, na.rm = FALSE),
      returns   = zoo::na.locf(returns, na.rm = FALSE)
    )
  ] %>% na.omit()

# View(fullData)

# Display raw data
fullData[
    ,
    .(day, year, cty_avg, roll_avg, temp, roll_temp, stocks, wei)
  ] %>%
  tidyr::gather(key, value, - year, -day) %>%
  ggplot(aes(x = day, y = value, group = year, col = year)) +
  geom_line() +
  facet_grid(vars(key), scales = "free")

#-----------------#
# Regression Data #
#-----------------#

cutDate <- as.Date("2019-05-13")

# Make train data
trainData <-
  fullData[
    !is.na(roll_temp) &
    !is.na(roll_avg) &
    date <= cutDate
  ]

# Make test data
testData <-
  fullData[
    !is.na(roll_temp) &
    !is.na(roll_avg) &
    date > cutDate
  ]

# Make ranger model
rangerModel <- ranger::ranger(cty_avg ~ month + temp, data = trainData)

# Predict 2020 values
newVals <- predict(rangerModel, data = fullData)$predictions
realVals <- fullData$roll_avg
deltaVals <- realVals - newVals

# Actual vs Predicted
data.table(
  date = fullData$date,
  real_vals = fullData$roll_avg,
  model_vals = predict(rangerModel, data = fullData)$predictions
)[,delta_vals := real_vals - model_vals] %>%
  tidyr::gather(key, value, -date) %>%
  dplyr::mutate(key = factor(key, levels = c("real_vals", "model_vals", "delta_vals", "stocks"))) %>%
  ggplot(aes(x = date, y = value, col = key)) +
  ggtitle(glue("Model Values vs Real Vals ({countryFltr})")) +
  xlab("Date") +
  ylab("No2 Levels") +
  geom_line() +
  geom_smooth() + 
  geom_vline(xintercept=cutDate, colour="grey")

# [,delta_vals := frollapply(delta_vals, n = 7, FUN = mean, fill = NA, align = "right")]

# Compare Facet
data.table(
  date = fullData$date,
  real_vals = realVals,
  model_vals =  predict(rangerModel, data = fullData)$predictions,
  temp = fullData$roll_temp
)[, delta_vals := deltaVals] %>%
  merge(spData[date >= "2018-04-30"], on = "date", all.x = TRUE) %>%
  tidyr::gather(key, value, -date) %>%
  dplyr::mutate(key = factor(key, levels = c("real_vals", "model_vals", "temp", "delta_vals", "stocks"))) %>%
  ggplot(aes(x = date, y = value, col = key)) +
  ggtitle(glue("{countryFltr} facset values")) +
  geom_line() +
  geom_smooth() +
  facet_grid(vars(key), scales = "free")

# Residual bars
# Actual vs Predicted
data.table(
  date = fullData$date,
  real_vals = fullData$roll_avg,
  new_vals = predict(rangerModel, data = fullData)$predictions
)[,delta_vals := real_vals - new_vals] %>%
  dplyr::select(date, delta_vals) %>%
  tidyr::gather(key, value, -date) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line()

#----------------#
# Predict Market #
#----------------#

stockModel <- ranger::ranger(stocks ~ roll_avg , trainData)

data.table(
  date = fullData$date,
  real_vals = fullData$stocks,
  model_vals = predict(stockModel, fullData)$predictions
) %>%
  tidyr::gather(key, value, -date) %>%
  ggplot(aes(x = date, y = value, group = key, col = key)) +
  geom_line()

