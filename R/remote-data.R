# devtools::install_github("tidyverse/ggplot2")
library(tidyverse)
library(sf)
library(dplyr)
library(DBI)

# https://github.com/justinelliotmeyers/official_chile_2017_census_boundary_shapefile

# 
# URL:      http://142.93.20.188/phpmyadmin
# Username: admin
# Password: 2623360fe6582dccff16ab38d509253389444f4e6237a6d8
# 
# CREATE DATABASE matpuc;
# CREATE USER 'test'@'%' IDENTIFIED BY 'HFW9KYZBnEYr!';
# GRANT ALL PRIVILEGES ON matpuc.* TO 'test'@'%';
# FLUSH PRIVILEGES;
# 

con <- dbConnect(
  RMySQL::MySQL(),
  dbname = "censo2017",
  host = "142.93.20.188", 
  port = 3306,
  user = "test",
  password = "HFW9KYZBnEYr!"
)

DBI::dbListTables(con) %>% 
  map(tbl, src = con) %>% 
  map(tally)

tbl(con, "hogar")

geoid <- tbl(con,"idgeo") %>% 
  select(COMUNA, DC, ID_ZONA_LOC) %>% 
  collect()

escolaridad <- tbl(con,"personas") %>%
  select(COMUNA, DC, ID_ZONA_LOC, P15,P09) %>%
  collect(n)

escolaridad <- right_join(escolaridad, geoid, by = c("ID_ZONA_LOC","COMUNA","DC"))



archivo_shape <- "~/../Downloads/official_chile_2017_census_boundary_shapefile-master/R13/Distritos_Censales.dbf"

st_layers(archivo_shape)

dcapa <- read_sf(archivo_shape, layer = "Distritos_Censales")
dcapa <- st_transform(dcapa, 32719)





