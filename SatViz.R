library(ncdf4)
library(ggplot2)
library(glue)
library(stringr)
library(purrr)
library(dplyr)
library(sp)
library(rworldmap)
library(magick)
library(future)
library(furrr)
library(fst)
library(data.table)

plan(multiprocess, workers = 4L)

source("satVizHelpers.R")

bboxCountry <- "CHN"

allFiles <- list.files("dataframes/all", pattern = ".fst$", full.names = TRUE)

source("countryBbox.R")

dir.create(glue("images/{bboxCountry}"), showWarnings = FALSE)

# future_map2(
walk2(
  allFiles,
  bboxCountry,
  function(x, bboxCountry){
    {
      
      threads_fst(2L)
      setDTthreads(4L)
      
      date  <- x %>% str_remove("dataframes/all/") %>% str_remove(".fst") %>% as.Date()
      year  <- format(date, "%Y")
      month <- format(date, "%m")
      day   <- format(date, "%d")
      
      vizData <- 
        read.fst(x, as.data.table = TRUE)[
          !is.na(lat) & !is.na(lon),
          `:=`(
            country = as.character(country),
            date    = date,
            year    = year,
            month   = month,
            day     = day
            # lat = round(lat, 1),
            # lon = round(lon, 1)
          )
        ]
      
      browser()
      
      vizData[country == "ITA"] %>% 
        ggplot(aes(x = lon, y = lat, fill = no2tc)) + 
        geom_tile(width = .1, height = .1)
        
      
      gg <- PlotRegion(vizData[,c("lat", "lon", "no2tc")], coords, "", limit = )
      
      ggsave(
        filename = glue("images/{bboxCountry}/NOX_{year}-{month}-{day}.png"), 
        plot = gg,
        width = 12L,
        height = 6L,
        dpi = "retina",
        units = "in"
      )
      NA_character_
    }
  }
)

allDates <- list.files(glue("images/{bboxCountry}"), recursive = TRUE, full.names = FALSE)

img <- image_graph(height = 1440L, width = 2880L, res = 226)
walk(
  allDates,
  ~{
    glue("images/{bboxCountry}/{.x}") %>% 
      image_read() %>% 
      image_annotate(.x, size = 140, gravity = "northwest", color = "black") %>% 
      plot()
    }
)
dev.off()
animation <- image_animate(img, fps = 7, optimize = TRUE)

plot(animation)
image_write(animation, glue("{bboxCountry}-animation.gif"))

