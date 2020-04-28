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
bboxCountry <- "CHN"

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
    list.files("dataframes/all", full.names = TRUE)[29],
    # safely({
      function(x){
        loopDate <- x %>% str_remove("dataframes/all/") %>% str_remove(".fst") %>% as.Date()
        print(loopDate)

        rawTbl <-
          read.fst(x, as.data.table = TRUE)[
            lon >= coords[1] & lat >= coords[2] & lon <= coords[3] & lat <= coords[4]
            ][
              ,
              `:=`(
                lat = round(lat * 10, 0) %>% as.integer(),
                lon = round(lon * 10, 0) %>% as.integer()
              )
            ][!is.na(no2tc)]

        # [,
        #   `:=`(
        #     date = loopDate,
        #     lat = round(lat, 1),
        #     lon = round(lon, 1)
        #   )
        # ]

        clusterGrid <-
          expand.grid(
            lat = seq(min(rawTbl$lat), max(rawTbl$lat), by = 1) %>% as.integer(),
            lon = seq(min(rawTbl$lon), max(rawTbl$lon), by = 1) %>% as.integer()
          )

        no2tc <- FNN::knn.reg(rawTbl[,-c("no2tc", "country")], clusterGrid, y = rawTbl$no2tc, k = 5)$pred

        cbind(clusterGrid, no2tc, date = loopDate)
      }
    # })
  ) %>%
  # map(~{.x$result}) %>%
  rbindlist()

setkey(rawData, date)
setindex(rawData, date, lat, lon)

# Manipulate raw data. approximate missing data. roll apploy over x days
satData <-
  rawData[
    order(date),
    `:=`(
      no2tc = frollapply(no2tc, n = 14, FUN = mean, fill = NA, align = "right") %>%
       as.integer()
    ),
    by = .(lat, lon)
  ][!is.na(no2tc)]

# Collect trash to take pressue off ram
rm(list = c("rawData")); gc()

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
  map( # for debugging in sequential
  # future_map(
    safely({
      function(x){

        loopDate  <- max(x$date)

        print(loopDate)

        x[
          ,
          `:=`(
            lat = as.numeric(lat) / 10,
            lon = as.numeric(lon) / 10,
          )
        ]

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
