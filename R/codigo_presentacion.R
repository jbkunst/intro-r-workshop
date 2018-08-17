## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  dev = "svg",
  cache = TRUE,
  cache.path = ".cache/",
  fig.path = "static/imgs/slides/",
  fig.width = 11,
  fig.height = 5
)

library(jbkmisc)
library(ggplot2)

theme_pres <- theme_jbk(
  base_family = "Roboto Condensed",
  plot_margin = margin(5, 5, 5, 5)
  ) + 
  theme(
    legend.position = "none",
    )

theme_set(theme_pres)

## ------------------------------------------------------------------------
library(tidyverse)

comunas <- read_csv("data/codigos_comunales.csv")
comunas

## ------------------------------------------------------------------------
library(haven)

casen <- read_sav("data/casen/Casen 2015.sav")
casen

## ---- cache=FALSE--------------------------------------------------------
library(dplyr)
library(DBI)

con <- dbConnect(
  RMySQL::MySQL(),
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

## ---- include=FALSE------------------------------------------------------
rm(casen)
gc()

## ------------------------------------------------------------------------
personas_resumen <- personas %>% 
  group_by(region, comuna) %>% 
  summarise(personas = n(), escolaridad_promedio = mean(ESCOLARIDAD)) %>% 
  collect()

personas_resumen

## ---- include=FALSE------------------------------------------------------
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

## ----results='hide'------------------------------------------------------
p <- ggplot(data) +
  geom_point(aes(x = ingreso_promedio_mm, y = escolaridad_promedio,
                 label = COMUNA))

## ---- echo=FALSE, fig.width = 6, fig.height = 7--------------------------
p

## ----results='hide'------------------------------------------------------
p <- ggplot(data) +
  geom_point(aes(x = ingreso_promedio_mm, y = escolaridad_promedio,
                 label = COMUNA, color = region, size= personas))

## ---- echo=FALSE, fig.width = 6, fig.height = 7--------------------------
p

## ----results='hide'------------------------------------------------------
p <- ggplot(data) +
  geom_point(aes(x = ingreso_promedio_mm, y = escolaridad_promedio,
                 label = COMUNA, color = region, size= personas),
             alpha = 0.75) +
  scale_color_viridis_d(option = "magma") +
  scale_x_continuous(trans = "log", labels = scales::comma,
                     breaks = seq(0, 1e3, by = 250))

## ---- echo=FALSE, fig.width = 6, fig.height = 7--------------------------
p

## ----results='hide'------------------------------------------------------
p <- ggplot(data) +
  geom_point(aes(x = ingreso_promedio_mm, y = escolaridad_promedio,
                 label = COMUNA, color = region, size= personas),
             alpha = 0.75) +
  scale_color_viridis_d(option = "magma") +
  scale_x_continuous(trans = "log", labels = scales::comma,
                     breaks = seq(0, 1e3, by = 250)) +
  facet_wrap(~region2)

## ---- echo=FALSE, fig.width = 6, fig.height = 7--------------------------
p

## ----results='hide'------------------------------------------------------
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

## ---- echo=FALSE, fig.width = 6, fig.height = 7--------------------------
p

## ---- echo=FALSE, fig.height=7-------------------------------------------
p

## ------------------------------------------------------------------------
library(sf)

# dgeo <- read_sf("data/R13/Comuna.shp", layer = "Comuna")
dgeo <- st_read("data/R13/Comuna.shp", layer = "Comuna")
dgeo

## ---- echo=FALSE---------------------------------------------------------
theme_set(theme_gray())
dgeo <- dgeo %>% mutate(COMUNA = as.numeric(as.character(COMUNA)))
# dgeo <- st_transform(dgeo, crs = 32719)

## ------------------------------------------------------------------------
ggplot() +
  geom_sf(data = dgeo) 

## ---- echo=FALSE---------------------------------------------------------
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

## ---- echo=FALSE, fig.height=7-------------------------------------------
p2

## ---- echo=FALSE, fig.height=7-------------------------------------------
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

## ---- echo=FALSE, fig.height=7-------------------------------------------
p3

## ------------------------------------------------------------------------
library(highcharter)
hchart(data, "point", hcaes(ingreso_promedio_mm, escolaridad_promedio,
                            group = region2, size = personas)) %>%
  hc_add_theme(hc_theme_smpl())

## ---- echo=FALSE---------------------------------------------------------
p <- p + theme_gray()

## ---- echo=TRUE----------------------------------------------------------
library(plotly)
ggplotly(p, height = 600) %>%
  config(displayModeBar = FALSE)

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

## ---- echo=FALSE, eval=FALSE---------------------------------------------
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

## ---- echo=FALSE, eval=FALSE---------------------------------------------
## htmlwidgets::saveWidget(md, file = "md.html", libdir = "libs", selfcontained = FALSE)

## ---- echo=FALSE, include=FALSE, cache=FALSE-----------------------------
knitr::purl("presentacion.Rmd", output = "R/codigo_presentacion.R", documentation = 1)

