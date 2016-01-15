require(gplots)         # for converting colors to HEX strings
require(grDevices)      # for color palettes
require(rgdal)          # for readOGR and others
require(sp)             # for spatial objects
require(leaflet)        # for interactive maps (NOT leafletR here)
require(dplyr)          # for working with data frames
require(ggplot2)        # for plotting
require(tigris)
require(acs)
require(stringr)        # to pad fips codes
require(purrr)
require(magrittr)
require(downloader)
require(tmap)
require(rgeos)
require(operator.tools) # for the `notin` function
require(tidyr)          # for the `spread` function
require(acs)            # for loading US Census data
require(readr)
require(rapport)        # for creating a camel case string
require(VGAM)           # for creating estimate median values for neighborhoods


options(scipen=999,stringsAsFactors = FALSE)

crs_proj <- CRS("+init=epsg:4326") # This project will use WGS 84 projected coordinate system
crs_geog <- CRS("+init=epsg:2285") # Washington State plane CRS