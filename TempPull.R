library(ncdf4)
library(glue)
library(purrr)
library(data.table)
library(fst)
library(ggplot2)

source("satVizHelpers.R")

downloadDates <- seq(from = as.Date("2020-04-01"), to = as.Date("2020-04-01"), by = 'month')

walk(
  downloadDates,
  function(x){
    
    print(x)
    
    month <- format(x, "%m")
    year <- format(x, "%Y")
    
    tempDir <- tempdir()
     
    system(glue("aws s3 cp s3://era5-pds/{year}/{month}/data/air_temperature_at_2_metres.nc {tempDir}/air_temperature_at_2_metres.nc"))
    
    path <- glue("{tempDir}/air_temperature_at_2_metres.nc")
    
    nc <- nc_open(path)
    
    temps <- ncvar_get(nc, "air_temperature_at_2_metres")
    
    lat <- nc$dim$lat$vals
    lon <- nc$dim$lon$vals - 180
    times <- nc$dim$time0$vals %>% as.POSIXct(origin = "1970-01-01")
    
    coordGrid <-
      expand.grid(
        lon = lon,
        lat = lat
      ) %>% as.data.table()
    
    coordGrid[,country := coords2country(data.frame(lon = lon, lat = lat))]
    
    combData <- 
      data.table(
        date_time = as.POSIXct(character()),
        country = factor(),
        temp = numeric()
      )
    
    for(i in (1:dim(temps)[3])){
      
      tempVals <- temps[,,i]
      
      loopTbl <- 
        data.table(
          date_time = times[i],
          lon = coordGrid$lon,
          lat = coordGrid$lat,
          country = coordGrid$country,
          temp = as.vector(tempVals)
        )[
          ,
          .(temp = mean(temp)),
          by = .(country, date_time)
          ]
      
      combData <- rbind(combData, loopTbl)
    }
    
    combData[,date := as.Date(date_time)]
    
    write.fst(combData, glue("dataframes/temps/{year}-{month}.fst"))
    
    gc()
    
  }
)


tempData <-
  map(
    list.files("dataframes/temps", full.names = TRUE),
    function(x){
      read.fst(x, as.data.table = TRUE)[,date:=NULL]
    }
  ) %>% 
  rbindlist() %>% 
  .[country == "USA"]


tempData <- tempData[,date := as.Date(date_time)][,
         .(
           temp = mean(temp)),
         by = .(date, country)]

tempData[
  order(date),
  temp := frollapply(temp, n = 14, FUN = mean, fill = NA, align = "right")
]

satData <-
  map(
    list.files("dataframes/all", full.names = TRUE),
    function(x){
      
      date <- x %>% str_remove("dataframes/all/") %>% str_remove(".fst") %>% as.Date()
      
      print(date)
      
      read.fst(x, as.data.table = TRUE)[
        country == "USA",
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
  
satData<- satData[
    order(date),
    cty_avg := frollapply(cty_avg, n = 14, FUN = mean, fill = NA, align = "right")
  ]

weiData <- readr::read_csv("dataframes/WEI.csv")
weiData <- as.data.table(weiData)[,.(date = DATE, wei = WEI)]

spData <- fread("dataframes/SP500.csv")[,.(date = as.Date(DATE), stocks = as.numeric(SP500))]

weiData[tempData, on = "date"][satData, on = "date"][spData, on = "date"]

  
fullData <-
  merge(tempData, satData, by = "date") %>% 
  merge(spData, by = "date") %>% 
  merge(weiData, on = "date", all = TRUE) %>% 
  .[
    order(date),
    wei := zoo::na.approx(wei)
  ]

fullData[,.(day, year, cty_avg, temp, stocks, wei)] %>% 
  tidyr::gather(key, value, - year, -day) %>% 
  ggplot(aes(x = day, y = value, group = year, col = year)) + geom_line() + 
  facet_grid(vars(key), scales = "free")

model <- lm(cty_avg ~ temp, data = tempData[satData, on = "date"][!is.na(cty_avg) & !is.na(temp)])

rangerModel <- ranger::ranger(cty_avg ~ temp + month, data = tempData[satData, on = "date"][,month:=lubridate::month(date)][!is.na(cty_avg) & !is.na(temp)])

newVals <- predict(rangerModel, data = tempData[satData, on = "date"][,month:=lubridate::month(date)][!is.na(cty_avg) & !is.na(temp)])$predictions
newVals <- predict(model, newdata = tempData[satData, on = "date"][!is.na(cty_avg) & !is.na(temp)])

econ <- tempData[satData, on = "date"][
  !is.na(cty_avg) & !is.na(temp),
 .(date, day, year, temp, cty_avg, new_vals = cty_avg - newVals)
]

econ %>% 
  merge(spData, on = "date") %>% 
  merge(weiData[date > "2018-04-01",], all = TRUE) %>%
  .[,`:=`(day = lubridate::yday(date),
          year = lubridate::year(date) %>% as.factor())] %>% 
  dplyr::select(-date, -test) %>% 
  tidyr::gather(key, value, -day, -year) %>% View()
ggplot(aes(x = day, y = value, group = year, col = year)) + 
  geom_line() +
  facet_grid(vars(key), scales = "free")


# ggplot(combData[country == "USA"], aes(y = lat, x = lon, fill = temp)) +
#   geom_raster() +
#   borders('world',
#           xlim = range(combData$lon), ylim = range(combData$lat),
#           colour='black', size = .5) +
#   theme_light() +
#   theme(panel.ontop = TRUE, panel.background = element_blank()) +
#   scale_fill_distiller(palette = "RdBu",
#                        na.value = "black"
#   ) 
