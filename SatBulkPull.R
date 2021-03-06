library(ncdf4)
library(ggplot2)
library(glue)
library(stringr)
library(purrr)
library(dplyr)
library(sp)
library(rworldmap)
library(fst)

source("satVizHelpers.R")

availDates <- list.files("dataframes/no2", pattern = ".fst") %>% str_remove(".fst") %>% as.Date()

# seqDates <- seq.Date(from = Sys.Date() - 3,  length.out = 5, by = "day") %>%
seqDates <- seq.Date(from = as.Date("2020-05-01"), to = Sys.Date() - 1, by = "day") %>% 
  .[!. %in% availDates]

# tempdir <- tempdir()
tempdir <- "SatelliteData"

walk(
  seqDates,
  safely({
    function(x){
      
      date <- as.Date(x, origin = "1970-01-01")
      
      print(date)
      
      year  <- format(date, format = "%Y")
      month <- format(date, format = "%m")
      day   <- format(date, format = "%d")
      
      # RPRO
      # OFFL
      # NRTI
      
      systemCalls <- glue::glue("aws s3 sync s3://meeo-s5p/NRTI/L2__NO2___/{year}/{month}/{day}/ {tempdir}/{year}/{month}/{day}")
      
      system(systemCalls)
      
      # set path and filename
      ncpath <- glue("{tempdir}/{year}/{month}/{day}")
      
      # declare dataframe
      no2df = NULL
      
      # get filenames
      no2files <- list.files(ncpath, pattern = "*nc$", full.names = TRUE)
      
      # save start time
      start.time <- Sys.time()
      
      # loop over filenames, open each one and add to dataframe
      for (i in seq_along(no2files)) {
        nc <- nc_open(no2files[i])
        
        mfactor <- ncatt_get(nc, "DETAILED_RESULTS/nitrogendioxide_total_column",
                             "multiplication_factor_to_convert_to_molecules_percm2")
        
        fillvalue <- ncatt_get(nc, "DETAILED_RESULTS/nitrogendioxide_total_column",
                               "_FillValue")
        
        qaValue <- ncvar_get(nc, "PRODUCT/qa_value")
        
        # get variables of interest
        no2tc <- ncvar_get(nc, "DETAILED_RESULTS/nitrogendioxide_total_column")
        tropo <- ncvar_get(nc, "PRODUCT/nitrogendioxide_tropospheric_column_precision")
        
        # apply multiplication factor for unit conversion
        
        lat <- ncvar_get(nc, "PRODUCT/latitude")
        lon <- ncvar_get(nc, "PRODUCT/longitude")
        
        time <- ncvar_get(nc, "PRODUCT/time_utc")
        
        df_loop <- 
          data.frame(
            time     = time,
            lat      = as.numeric(lat),
            lon      = as.numeric(lon),
            no2tc    = as.vector(no2tc) * mfactor$value,
            qa_value = as.vector(qaValue),
            tropo    = as.vector(tropo)
          ) %>%
          filter(no2tc != fillvalue & qa_value > .50)
        
        # concatenate the new data to the global data frame
        no2df <- rbind(no2df, df_loop)
        
        # close file
        nc_close(nc)
      }
      
      countryCords <-
        no2df %>%
        filter(!is.na(lat) & !is.na(lon)) %>%
        mutate(country = coords2country(data.frame(lon, lat))) 
      
      
      write.fst(countryCords, glue("dataframes/no2/{date}.fst"), compress = 100)
      
      system(glue("rm -r {tempdir}"))
      
    }
  })
)
