library(tidycensus)
library(mapdeck)
library(tidyverse)
library(sf)
token <- "pk.eyJ1IjoiamJrdW5zdCIsImEiOiJjamt4OTRmZXMwNjhoM3FxamNya2pqNHZjIn0.2FkUN09oWHz0Jg07MzyKKA"

census_api_key("35f116582d5a89d11a47c7ffbfc2ba309133f09d", overwrite = TRUE, install = TRUE)


dgeo <- read_sf("data/R13/Comuna.shp")
class(dgeo)


dgeo <- st_read("data/R13/Comuna.shp")
class(dgeo)

# hv <- get_acs(geography = "tract", 
#               variables = "B25077_001", 
#               state = "CA", 
#               geometry = TRUE) %>%
#   drop_na(estimate) %>%
#   mutate(elev = estimate/ 100)

mapdeck(token = token) %>%
  add_polygon(data = dgeo, 
              # fill_colour = "estimate", 
              # tooltip = "estimate", 
              stroke_width = 0, 
              layer = "Comuna",
              # elevation = "elev")
  )
