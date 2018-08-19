## install.packages(c("tidyverse", "haven", "dplyr", "DBI", "RMySQL", "sf", "classInt", "broom", "ggrepel", "plotly", "highcharter", "leaflet", "mapdeck"))

## ------------------------------------------------------------------------
library(tidyverse)

comunas <- read_csv("data/codigos_comunales.csv")
comunas

## ------------------------------------------------------------------------
library(haven)

casen <- read_sav("data/casen/Casen 2015.sav")
casen

## ------------------------------------------------------------------------
library(dplyr)
library(DBI)
library(RMySQL)

con <- dbConnect(
  drv = MySQL(),
  dbname = "censo2017",
  host = "142.93.20.188", 
  port = 3306,
  user = "test",
  password = "HFW9KYZBnEYr!"
)

dbListTables(con)

## ------------------------------------------------------------------------
personas <- tbl(con,"personas")
personas

## ------------------------------------------------------------------------
casen_comuna <- casen %>% 
  mutate(comuna = as.numeric(comuna)) %>% 
  group_by(comuna) %>% 
  summarise(ingreso_promedio_mm = mean(y1, na.rm = TRUE)/1000)
casen_comuna

## ------------------------------------------------------------------------
rm(casen)
gc()

## ------------------------------------------------------------------------
personas_resumen <- personas %>% 
  group_by(region, comuna) %>% 
  summarise(personas = n(), escolaridad_promedio = mean(ESCOLARIDAD)) %>% 
  collect()

personas_resumen

## ------------------------------------------------------------------------
comunas <- comunas %>% mutate(COMUNA = as.character(COMUNA))

## ------------------------------------------------------------------------
data <- comunas %>%
  inner_join(personas_resumen, by = c("CODIGO" = "comuna")) %>% 
  inner_join(casen_comuna, by = c("CODIGO" = "comuna"))

data

## ------------------------------------------------------------------------
data <- data %>% 
  mutate(
    region = factor(region),
    region2 = fct_lump(region, n = 8)
    )

data

## ------------------------------------------------------------------------
p <- ggplot(data) +
  geom_point(aes(x = ingreso_promedio_mm, y = escolaridad_promedio,
                 label = COMUNA))

## ------------------------------------------------------------------------
p

## ------------------------------------------------------------------------
p <- ggplot(data) +
  geom_point(aes(x = ingreso_promedio_mm, y = escolaridad_promedio,
                 label = COMUNA, color = region, size= personas))

## ------------------------------------------------------------------------
p

## ------------------------------------------------------------------------
p <- ggplot(data) +
  geom_point(aes(x = ingreso_promedio_mm, y = escolaridad_promedio,
                 label = COMUNA, color = region, size= personas),
             alpha = 0.75) +
  scale_color_viridis_d(option = "magma") +
  scale_x_continuous(trans = "log", labels = scales::comma,
                     breaks = seq(0, 1e3, by = 250))

## ------------------------------------------------------------------------
p

## ------------------------------------------------------------------------
p <- ggplot(data) +
  geom_point(aes(x = ingreso_promedio_mm, y = escolaridad_promedio,
                 label = COMUNA, color = region, size= personas),
             alpha = 0.75) +
  scale_color_viridis_d(option = "magma") +
  scale_x_continuous(trans = "log", labels = scales::comma,
                     breaks = seq(0, 1e3, by = 250)) +
  facet_wrap(~region2)

## ------------------------------------------------------------------------
p

## ------------------------------------------------------------------------
p <- ggplot(data) +
  geom_point(aes(x = ingreso_promedio_mm, y = escolaridad_promedio,
                 label = COMUNA, color = region, size= personas),
             alpha = 0.75) +
  scale_color_viridis_d(option = "magma") +
  scale_x_continuous(trans = "log", labels = scales::comma,
                     breaks = seq(0, 1e3, by = 250)) +
  facet_wrap(~region2) +
  geom_smooth(aes(x = ingreso_promedio_mm, y = escolaridad_promedio),
              method = "lm", se = FALSE, color = "red", size = 1.2)

## ------------------------------------------------------------------------
p

## ------------------------------------------------------------------------
p

## ------------------------------------------------------------------------
library(sf)
dgeo <- st_read("data/shapes/R13/Comuna.shp", layer = "Comuna", quiet = TRUE)
dgeo

## ------------------------------------------------------------------------
theme_set(theme_gray())
dgeo <- dgeo %>% mutate(COMUNA = as.numeric(as.character(COMUNA)))
# dgeo <- read_sf("data/R13/Comuna.shp", layer = "Comuna")
# dgeo <- st_transform(dgeo, crs = 32719)


## ------------------------------------------------------------------------
ggplot() +
  geom_sf(data = dgeo)

## ------------------------------------------------------------------------
classint <- function(x, labels = NULL, ...) {
  cut(x, breaks = classIntervals(x, ...)$brks, include.lowest = TRUE, labels = labels)
}

## ------------------------------------------------------------------------
library(classInt)
niveles <- c("bajo", "medio", "alto")

dgeo <- dgeo %>% 
  left_join(data, by = c("COMUNA" = "CODIGO")) %>% 
  mutate(
    escolaridad = classint(escolaridad_promedio, n = 3, style = "kmeans", labels = niveles), 
    ingreso = classint(ingreso_promedio_mm, n = 3, style = "kmeans", labels = niveles)
  )

glimpse(dgeo)

## ------------------------------------------------------------------------
p2 <- ggplot() +
  geom_sf(data = select(dgeo, COMUNA, geometry),
          fill = "gray95", color = "gray80", size = 0.1) +
  geom_sf(data = dgeo, fill = "darkred", color = "gray80", size = 0.1) +
  scale_fill_viridis_d() +
  facet_grid(ingreso ~ escolaridad) +
  theme_minimal() 

## ------------------------------------------------------------------------
p2

## ------------------------------------------------------------------------
p

## ------------------------------------------------------------------------
data

## ------------------------------------------------------------------------
datag <- data %>% 
  group_by(region2) %>% 
  nest()

datag

## ------------------------------------------------------------------------
library(broom)

datag <- datag %>% 
  mutate(
    modelo = map(data, lm, formula = escolaridad_promedio ~ ingreso_promedio_mm),
    parametros = map(modelo, tidy)
  )

datag

## ------------------------------------------------------------------------
dmods <- datag %>% 
  select(region2, parametros) %>% 
  unnest()

dmods

## ------------------------------------------------------------------------
dmods <- dmods %>% 
  select(region2, term, estimate) %>% 
  spread(term, estimate) 

dmods

## ------------------------------------------------------------------------
library(ggrepel)

p3 <- ggplot(dmods, aes(ingreso_promedio_mm, `(Intercept)`)) +
  geom_point(size = 7, alpha = 0.6, color = "gray60") +
  geom_text_repel(aes(label = region2), force = 20) +
  scale_x_continuous(limits = c(0, NA))

## ------------------------------------------------------------------------
p3

## ------------------------------------------------------------------------
p <- p + theme_gray()

## ------------------------------------------------------------------------
library(plotly)
ggplotly(p, height = 600) %>%
  config(displayModeBar = FALSE)

## ------------------------------------------------------------------------
comuna_tipohogar <- tbl(con, "hogar") %>% 
  count(COMUNA, TIPO_HOGAR) %>% 
  collect(n = Inf) %>% 
  mutate(
    TIPO_HOGAR = case_when(
      TIPO_HOGAR == 1 ~ "unipersonal",
      TIPO_HOGAR == 2 ~ "nuclear monoparental",
      TIPO_HOGAR == 3 ~ "nuclear biparental sin hijos",
      TIPO_HOGAR == 4 ~ "nuclear biparental con hijos",
      TIPO_HOGAR == 5 ~ "compuesto",
      TIPO_HOGAR == 6 ~ "extenso",
      TIPO_HOGAR == 7 ~ "sin núcleo",
      TRUE ~ "NA"
    )
  ) %>% 
  rename(name = TIPO_HOGAR, y = n) %>% 
  nest() %>% 
  rename(ttdata = data) %>% 
  mutate(ttdata = map(ttdata, highcharter::list_parse)) 

data <- left_join(data, comuna_tipohogar, by = c("CODIGO" = "COMUNA"))
data

## ------------------------------------------------------------------------
library(highcharter)

hc <- hchart(
  data, type = "point",
  hcaes(ingreso_promedio_mm, escolaridad_promedio, group = region2, size = personas),
  minSize = 1, maxSize = 15
  ) %>%
  hc_xAxis(
    type = "logarithmic", crosshair = TRUE,
    labels = list(format = "${value}")) %>%
  hc_colors(hex_to_rgba(viridis::cividis(9))) %>% 
  hc_tooltip(
    useHTML = TRUE,
    pointFormatter = tooltip_chart(
      accesor = "ttdata",
      hc_opts = list(
        chart = list(type = "pie"),
        xAxis = list(type = "category"),
        title = list(text = "point.COMUNA")
        )
    )
  ) %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_size(height = 700)

hc

## ------------------------------------------------------------------------
## htmlwidgets::saveWidget(hc, file = "hc.html", libdir = "libs", selfcontained = FALSE)

## ------------------------------------------------------------------------
library(leaflet)

pal <- colorNumeric("viridis", NULL)

l <- leaflet(dgeo) %>%
  addTiles() %>%
  addPolygons(
    stroke = TRUE, color = "#DADADA", weight = 1,
    smoothFactor = 2, fillOpacity = 0.75,
    fillColor = ~pal(escolaridad_promedio),
    label = ~paste0(DESC_COMUN, ": ", escolaridad_promedio)
    ) %>%
  addLegend(pal = pal, values = ~escolaridad_promedio, opacity = 1.0)
l

## ------------------------------------------------------------------------
## htmlwidgets::saveWidget(l, file = "l.html", libdir = "libs", selfcontained = FALSE)

## ------------------------------------------------------------------------
library(mapdeck)

token <- "pk.eyJ1IjoiamJrdW5zdCIsImEiOiJjamt4OTRmZXMwNjhoM3FxamNya2pqNHZjIn0.2FkUN09oWHz0Jg07MzyKKA"

dgeo <- mutate(dgeo, personas_cientos = personas/100)

md <- mapdeck(token = token, height = 700,
               zoom = 7, location = c(-70.65664, -33.45327),
              style = 'mapbox://styles/mapbox/dark-v9', pitch = 45) %>%
  add_polygon(
    data = dgeo, 
    fill_colour = "escolaridad_promedio", 
    tooltip = "DESC_COMUN", 
    stroke_width = 0, 
    layer = "Comuna",
    elevation = "personas_cientos"
    )
md

## ------------------------------------------------------------------------
## htmlwidgets::saveWidget(md, file = "md.html", libdir = "libs", selfcontained = FALSE)

## ------------------------------------------------------------------------
