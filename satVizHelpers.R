PlotRegion <- function(df, latlon, title, limits, stops) {
  # Plot the given dataset over a geographic region.
  #
  # Args:
  #   df: The dataset, should include the no2tc, lat, lon columns
  #   latlon: A vector of four values identifying the botton-left and top-right corners
  #           c(latmin, latmax, lonmin, lonmax)
  #   title: The plot title

  # subset the data frame first
  subtitle = paste("Data min =", formatC(min(df$no2tc, na.rm = TRUE), format = "e", digits = 2),
                   "max =", formatC(max(df$no2tc, na.rm = TRUE), format = "e", digits = 2))

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
    coord_quickmap(xlim = c(latlon[1], latlon[3]), ylim = c(latlon[2], latlon[4])) +
    labs(title=title, subtitle = subtitle,
         x = "Longitude", y = "Latitude",
         fill = expression(molecules~cm^-2))
}

coords2country = function(points)
{
  
  proj4string <- sp::proj4string
  
  countriesSP <- rworldmap::getMap(resolution = 'coarse')
  # countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail

  # convert our list of points to a SpatialPoints object

  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

  #setting CRS directly to that from rworldmap
  pointsSP = sp::SpatialPoints(points, proj4string = sp::CRS(proj4string(countriesSP)))


  # use 'over' to get indices of the Polygons object containing each point
  indices = sp::over(pointsSP, countriesSP)

  # return the ADMIN names of each country
  # indices$ADMIN
  indices$ISO3 # returns the ISO3 code
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}