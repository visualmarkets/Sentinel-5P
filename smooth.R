library(data.table)
library(fst)
library(ggplot2)

# Params
bboxCountry <- "GBR"

# Pull in bboxCountry cooridnates
source("countryBbox.R")

df <- read.fst("dataframes/all/2018-04-30.fst", as.data.table = TRUE)[
  lon >= coords[1] & lat >= coords[2] & lon <= coords[3] & lat <= coords[4],
][
  ,
  `:=`(
    lat = round(lat, 2),
    lon = round(lon, 2),
    no2tc = round(no2tc, 0)
  )
  ][
    ,
    .(no2tc = mean(no2tc)),
    by = .(lat, lon)
    ]

test <- 
  expand.grid(
    lat = seq(min(df$lat), max(df$lat), by = 0.01),
    lon = seq(min(df$lon), max(df$lon), by = 0.01)
  )



no2tc <- FNN::knn.reg(df[,-c("no2tc")], test, y = df$no2tc, k = 3)$pred

df2 <- cbind(test, no2tc)


# Set color stops for the ggplot2 axis
stops <- quantile(df,
                  c(0, 0.4, .7, .98,0.9999),
                  na.rm = TRUE
)

# Set limits for color axis. Missing values will be black
limits <- c(head(stops,1), tail(stops,1))

ggplot(df, aes(y = lat, x = lon, fill = no2tc)) +
  geom_raster() +
  borders('world',
          xlim = range(df$lon), ylim = range(df$lat),
          colour='black', size = .5) +
  theme_light() +
  theme(panel.ontop = TRUE, panel.background = element_blank()) +
  scale_fill_distiller(palette = "RdYlGn",
                       limits = limits,
                       na.value = "black",
                       values = scales::rescale(stops)
  ) +
  coord_quickmap(xlim = c(coords[1], coords[3]), ylim = c(coords[2], coords[4]))
