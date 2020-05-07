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
