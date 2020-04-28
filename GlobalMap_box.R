#--------------#
# Script Setup #
#--------------#

# Load Libs
library(ncdf4)
library(ggplot2)
library(glue)
library(stringr)
library(purrr)
library(dplyr)
library(sp)
library(rworldmap)
library(magick)
library(fst)
library(data.table)
library(ggplot2)
library(future)
library(furrr)

# Set Future Plan
plan(multiprocess, workers = 6L)

# Source Helpers
source("satVizHelpers.R")

# Params
bboxCountry <- "ITA"

# Pull in bboxCountry cooridnates
source("countryBbox.R")

# List files
allFiles <- list.files("dataframes/all", pattern = ".fst$", full.names = TRUE)

# Create Dirs for bbox
dir.create(glue("images"), showWarnings = FALSE)
dir.create(glue("images/pngs"), showWarnings = FALSE)
dir.create(glue("images/gifs"), showWarnings = FALSE)
dir.create(glue("images/pngs/{bboxCountry}"), showWarnings = FALSE)

#--------------#
# Compute Data #
#--------------#

# Read in raw data & manipulate
rawData <-
  map(
    list.files("dataframes/all", full.names = TRUE),
    function(x){
      loopDate <- x %>% str_remove("dataframes/all/") %>% str_remove(".fst") %>% as.Date()
      print(loopDate)
      read.fst(x, as.data.table = TRUE)[
        lon >= coords[1] & lat >= coords[2] & lon <= coords[3] & lat <= coords[4]
      ][,
        `:=`(
          date = loopDate,
          lat = round(lat, 1),
          lon = round(lon, 1)
        )
      ][
        ,
        .(no2tc = mean(no2tc)),
        by = .(lat, lon, date)
      ]
    }
  ) %>%
  rbindlist()

# Make grid of all lat lon cooridates with date. This is for the approximation of na values
fullGrid <-
  expand.grid(
      lat = unique(rawData$lat),
      lon = unique(rawData$lon),
      date = seq(min(rawData$date), max(rawData$date), by = "day")
    ) %>%
  as.data.table()

# Manipulate raw data. approximate missing data. roll apploy over x days
satData <-
  rawData[
    fullGrid, on = .(lat, lon, date)
  ][
    order(date),
    `:=`(no2tc = zoo::na.approx(no2tc, na.rm = FALSE)),
    by = .(lat, lon)
  ][
    ,
    `:=`(
      no2tc = frollapply(no2tc, n = 14, FUN = mean, fill = NA, align = "right")
    ),
    by = .(lat, lon)
  ][!is.na(no2tc) & !outliers::outlier(no2tc, logical = TRUE),]

# Collect trash to take pressue off ram
rm(list = c("rawData", "fullGrid")); gc()

# Set keys and index for data.table faster querying
setkey(satData, date)
setindex(satData, date)

# Set color stops for the ggplot2 axis
stops <- quantile(satData$no2tc,
                  # seq(0, 1, .01),
                  c(0, 0.7, .8, .98, .99,0.9999),
                  na.rm = TRUE
                  )

# Set limits for color axis. Missing values will be black
limits <- c(head(stops,1), tail(stops,1))

# Data for bottom half line exhibit
lineData <- satData[
    # country == bboxCountry,
    ,
    .(avg = mean(no2tc)),
    by = .(date)
  ]

# Map through satData by date. Parallelized
satData %>%
split(by = 'date') %>%
# map( # for debugging in sequential
future_map(
  safely({
    function(x){
      loopDate  <- max(x$date)
      test <- 
        expand.grid(
          lat = seq(min(x$lat), max(x$lat), by = 0.01),
          lon = seq(min(x$lon), max(x$lon), by = 0.01)
        )
      
      no2tc <- FNN::knn.reg(x[,-c("no2tc", "date")], test, y = x$no2tc, k = 3)$pred
      
      x <- cbind(test, no2tc)
      
      

      print(loopDate)

      gg <-
        PlotRegion(df = x[,c("lat", "lon", "no2tc")], coords, glue::glue("Date: {loopDate}"), limits, stops) +
        theme(legend.position = "right")

      line <- lineData[date <= loopDate]

      vv <-
        ggplot(line, aes(x = date, y = avg)) +
        geom_line() +
        geom_smooth(method = 'loess', formula = y ~ x)

      figure <- ggpubr::ggarrange(gg, vv,
                                  ncol = 1, nrow = 2,
                                  heights = c(2.5, 1))

      ggsave(
        filename = glue::glue("images/pngs/{bboxCountry}/NO2_{loopDate}.png"),
        plot = figure,
        width = 6L,
        # height = 6L,
        dpi = "print",
        units = "in"
      )
      gc()
      NA_character_
    }
  })
)

gc()

#--------------#
# Write Images #
#--------------#

# Read png files
allDates <- list.files(glue("images/pngs/{bboxCountry}"), recursive = TRUE, full.names = FALSE)

# Read pngs and make animation
animation <-
  glue("images/pngs/{bboxCountry}/{allDates}") %>%
  image_read() %>%
  image_scale("800") %>%
  image_animate(fps = 20, optimize = TRUE)

fileName <- glue("images/gifs/{bboxCountry}-animation.gif")

unlink(fileName); gc()

# Write gif
image_write(animation, fileName)
