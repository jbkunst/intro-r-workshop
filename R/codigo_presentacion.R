## install.packages(c("tidyverse", "haven", "DBI", "RMySQL", "sf", "classInt", "broom", "ggrepel", "smallvis", "plotly", "highcharter", "leaflet", "mapdeck"))

## ------------------------------------------------------------------------
library(tidyverse) # combinacion de paquetes para importar + transformar + visualizar

comunas <- read_csv("data/codigos_comunales.csv")
comunas

## ------------------------------------------------------------------------
library(haven) # SPSS SAS STATA

casen <- read_sav("data/casen/Casen 2015.sav")
casen

## ------------------------------------------------------------------------
library(DBI)    # Interfaz para bases de datos
library(RMySQL) # Driver para MySQL

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
library(sf) # datos espaciales
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
library(classInt) # ayuda a crear intervalos 
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
  geom_sf(data = dgeo, aes(fill = escolaridad_promedio), color = "gray80", size = 0.1) +
  scale_fill_viridis_c(option = "B") +
  facet_grid(ingreso ~ escolaridad) +
  theme_minimal() +
  labs(x = "Ingreso", y = "Escolaridad")

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
library(broom) # lleva modelos/test a data frames

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
library(ggrepel) # extension ggplot2 que crea etiquetas que se repelen

p3 <- ggplot(dmods, aes(ingreso_promedio_mm, `(Intercept)`)) +
  geom_point(size = 8, color = "darkred", alpha = 0.6, shape = 16) +
  geom_text_repel(aes(label = region2), force = 20) +
  scale_x_continuous(limits = c(0, NA)) 

## ------------------------------------------------------------------------
p3

## ------------------------------------------------------------------------
casen

## ------------------------------------------------------------------------
vars <- c("region", "comuna", 
          "tot_hog", "tot_par", 
          "tot_nuc", "tot_per", 
          "sexo", "edad", 
          "ecivil", "pareja", 
          "e1", "e2a", 
          "o1", "o2", 
          "y1", "y2d", 
          "y3a", "y27a", 
          "y27b", "y27c", 
          "y27d", "y27e", 
          "s4", "s5", 
          "s12", "s13", 
          "r1a", "v1", 
          "v2", "v8", 
          "v10", "v11", 
          "v13", "v18", 
          "v19", "v23", 
          "v26", "y0101", 
          "y0301", "ESC", 
          "educ", "depen", 
          "hacinamiento")

# variables de interes
casen <- select(casen, vars)

casen <- casen %>% 
  mutate_if(is.labelled, as_factor) %>% 
  mutate_at(vars(y1, y2d, s4, s5, v8), function(x) as.numeric(as.character(x)))

# casen num  
casen_num <- casen %>% 
  group_by(region, comuna) %>% 
  select_if(is.numeric) %>% 
  ungroup()

# casen factor
casen_fac <- casen %>% 
  group_by(region, comuna) %>% 
  select_if(is.factor) %>% 
  ungroup() %>% 
  mutate_all(fct_explicit_na, na_level = "NA") %>% 
  mutate_at(vars(-1, -2), fct_lump, other_level = "Otra") 

nms <- names(casen_fac)
casen_fac <- map2_dfc(casen_fac, nms, function(v, n){
  levels(v) <- paste(n, levels(v), sep = "_")
  v
})

casen_fac <- casen_fac %>% 
  mutate_at(vars(1, 2), as.character) %>% 
  mutate_at(vars(1, 2), str_remove, "^.*_")

# casen num comuna
casen_num_c <- casen_num %>% 
  group_by(region, comuna) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  mutate_all(replace_na, 0) %>% 
  ungroup()

table_list <- function(x) {
  # x <- sample(LETTERS[1:5], 20, 1:5, replace = TRUE)
  t <- table(x)
  
  data_frame(
    cat = names(t),
    val = as.numeric(prop.table(t))
  ) %>% 
    spread(cat, val) %>% 
    list()
}

# casen fac comuna
casen_fac_c <- casen_fac %>% 
  group_by(region, comuna) %>% 
  summarise_all(table_list) %>% 
  unnest()

casen_c <- left_join(casen_num_c, casen_fac_c)

## ------------------------------------------------------------------------
vars

## ------------------------------------------------------------------------
casen_c

## ------------------------------------------------------------------------
library(smallvis)
umap_casen_c <- smallvis(casen_c, method = "umap", perplexity = 25, eta = 0.01, verbose = FALSE)

head(umap_casen_c)

## ------------------------------------------------------------------------
casen_umap <- casen_c %>% 
  select(region, comuna) %>% 
  bind_cols(as.data.frame(umap_casen_c))
casen_umap

## ------------------------------------------------------------------------
p4 <- ggplot(casen_umap, aes(V1, V2, color = region, label = comuna)) +
  geom_point() +
  scale_color_viridis_d() +
  theme_void() + 
  theme(legend.position = "bottom")

## ------------------------------------------------------------------------
p4

## ------------------------------------------------------------------------
p5 <- p4 +
  geom_text_repel(size = 2, color = "black", alpha = 0.75, segment.colour = "black",
                  segment.alpha = 0.5, segment.size = 0.25) +
  facet_wrap(~region) +
  theme(legend.position = "none")

## ------------------------------------------------------------------------
p5

## ------------------------------------------------------------------------
p <- p + theme_gray()

## ------------------------------------------------------------------------
library(plotly) # htmlwidgets de plotlyJS
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
library(highcharter) # htmlwidgets de highcharts
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
library(leaflet) # htmlwidget de leafletJS

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
library(mapdeck) # hmtlwidget Mapbox + Deck.gl 

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
cat(paste(readLines("rmarkdown-test.Rmd", encoding = "UTF-8"), collapse = "\n"))

## ------------------------------------------------------------------------
cat(paste(readLines("app.R", encoding = "UTF-8"), collapse = "\n"))

## ------------------------------------------------------------------------
