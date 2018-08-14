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

DBI::dbListTables(con)

DBI::dbListTables(con) %>% 
  map(tbl, src = con)

DBI::dbListTables(con) %>% 
  map(tbl, src = con) %>% 
  map(tally)

tbl(con, "hogar")

geoid <- tbl(con,"idgeo") %>% 
  select(COMUNA, DC, ID_ZONA_LOC) %>% 
  collect(n = Inf)

escolaridad <- tbl(con,"personas") %>%
  select(COMUNA, DC, ID_ZONA_LOC, P15,P09) %>%
  collect(n = Inf)

escolaridad %>% filter(!is.na(P15))

escolaridad <- right_join(escolaridad, geoid, by = c("ID_ZONA_LOC","COMUNA","DC"))
escolaridad

escolaridad_comuna <- escolaridad %>%
  filter(COMUNA %in% c("13114"))

escolaridad_comuna_dc <- escolaridad_comuna %>% 
  group_by(DC) %>% 
  summarise_all(mean, rm.na = TRUE)

archivo_shape <- "~/../Downloads/official_chile_2017_census_boundary_shapefile-master/R13/Distritos_Censales.dbf"

st_layers(archivo_shape)

dcapa <- read_sf(archivo_shape, layer = "Distritos_Censales")
dcapa <- st_transform(dcapa, 32719)

dcapa <- dcapa %>% 
  left_join(escolaridad_comuna_dc %>% mutate(COMUNA = as.character(COMUNA)), by = c("COMUNA", "COD_DISTRI" = "DC"))

ggplot() +
  geom_sf(data = dcapa, size = .2, fill = NA, colour = "gray80")

plot(dcapa %>% filter(!is.na(P15)))

p <- dcapa %>% 
  filter(!is.na(P15)) %>% 
  ggplot() +
  geom_sf(aes(fill = P15), colour = NA) +
  geom_sf(data = st_centroid(dcapa %>% filter(!is.na(P15))), aes(geometry = geometry, size = COD_DISTRI)) +
  theme_minimal()

p

plotly::ggplotly(p)

rmapshaper::ms_simplify(input = as(dcapa, 'Spatial')) %>%
  st_as_sf() %>% 
  ggplot() +
  geom_sf(data = dcapa, aes(fill = P15), colour = NA) +
  theme_bw()

# otro --------------------------------------------------------------------
archivo_shape <- "~/../Downloads/official_chile_2017_census_boundary_shapefile-master/R13/Comuna.shp"

st_layers(archivo_shape)

dcapa <- read_sf(archivo_shape, layer = "Comuna")
dcapa <- st_transform(dcapa, 32719)

ggplot() +
  geom_sf(data = dcapa, size = .2, fill = NA, colour = "gray80")
