library(tidyverse)
library(fst)
library(data.table)
library(forecast)

ip <- readr::read_csv("dataframes/IPMAN.csv")

satData <-
  map(
    list.files("dataframes/all", full.names = TRUE),
    function(x){

      date <- x %>% str_remove("dataframes/all/") %>% str_remove(".fst") %>% as.Date()

      print(date)

      read.fst(x, as.data.table = TRUE)[
        country == "USA" |
        country == "CHN" |
        country == "ITA" |
        country == "DEU" |
        country == "IND",
        .(cty_avg = mean(no2tc)),
        by = country
      ][,
        `:=`(date = date,
             year = lubridate::year(date) %>% as.factor(),
             day  = lubridate::yday(date)
        )
      ]
    }
  ) %>%
  rbindlist()

setkey(satData, country, date)
setindex(satData, country)

vizData <-
  satData[
      ,
      `:=`(
        roll_avg = frollapply(cty_avg, n = 14, FUN = mean, fill = NA, align = "right")
      ),
      by = .(country)
    ][
      !is.na(roll_avg)
    ]

vizData %>%
  ggplot(aes(x = date, y = log(roll_avg))) +
  geom_line() +
  geom_smooth(se = FALSE, formula = y ~ x, method = "loess") +
  facet_grid(cols = vars(country))


# 23 Wuhan lockdowns
# 58 Lombardy lockdowns

vizData[day < 101] %>%
  ggplot(aes(x = day, y = log(roll_avg), group = year, col = year)) +
  geom_line() +
  geom_smooth(se = FALSE, formula = y ~ x, method = "loess") +
  facet_grid(cols = vars(country)) +
  geom_vline(xintercept=23, colour="grey") +
  geom_vline(xintercept=58, colour="grey")


vizData %>% 
  ggplot(aes(x = day, y = roll_avg, group = year, col = year)) + 
  geom_line() + 
  facet_grid(rows = vars(country), scales = "free") +
  geom_vline(xintercept=23, colour="grey") +
  geom_vline(xintercept=58, colour="grey")

# Difference from last year
dcast(vizData, day + country ~ year , value.var = "roll_avg")[
  !is.na(`2019`) & !is.na(`2020`)][
    ,
    delta := `2020` - `2019`
  ] %>%
  ggplot(aes(x = day, y = delta, group = country, col = country)) +
  geom_smooth()

# Quarter box plots
vizData[
  ,
  `:=`(month = lubridate::month(date) %>% as.factor(),
       year = as.factor(year),
       day = as.factor(day),
       quarter = lubridate::quarter(date) %>% as.factor(),
       year_qtr = zoo::as.yearqtr(date))
] %>%
  ggplot(aes(x = year, y = roll_avg, fill = year)) +
  geom_boxplot() +
  facet_grid(rows = vars(country), cols = vars(month), scales = "free")



# ip <- ip %>% mutate(IPMAN = IPMAN / ip[[1,'IPMAN']])
# no2tcStart <- satData[,.(no2tc = mean(no2tc)),by = date][1]$no2tc
#
# econData <- merge(satData[,.(no2tc = mean(no2tc) / no2tcStart),by = date], ip, by.x = "date", by.y = "DATE") %>%
#   melt.data.table(id.vars = "date", measure.vars = c("IPMAN", "no2tc"))
#
# ggplot(econData[variable =="IPMAN"], aes(date, y = value)) + geom_line() +
#   facet_grid(vars(variable))

map(
  unique(vizData$country),
  function(x){

    vizData <- vizData[country == x, .(date, roll_avg)]

    tsData <-
      merge(
        data.table(date = seq.Date(from = min(vizData$date), to = max(vizData$date), by = "day")),
        vizData,
        all.x = TRUE
      )[
        ,
        .(date,
          roll_avg = zoo::na.approx(roll_avg))
      ]

    tsTrain <- tsData[date < "2020-01-01"]
    tsTest <- tsData[date >= "2020-01-01"][,.(date_plot = date, roll_avg)]

    trainMinYear <- lubridate::year(min(tsTrain$date))
    trainMinDay  <- lubridate::yday(min(tsTrain$date))

    tsTrain <- ts(tsTrain$roll_avg, freq = 365, start = c(trainMinYear, trainMinDay))

    testMinYear <- lubridate::year(min(tsTest$date))
    testMinDay <- lubridate::yday(min(tsTest$date))

    tsTest <- ts(tsTest$roll_avg, freq = 365, start = c(testMinYear, testMinDay))

    tsTrain %>%
    ets() %>%
    forecast(h = 30, level = 80) %>%
    autoplot() +
    autolayer(tsTest) + ggtitle(x)
  }
)
