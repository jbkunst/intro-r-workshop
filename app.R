library(shiny)
library(tidyverse)
library(sf)
library(ggrepel)
library(leaflet)
library(plotly)
data <- readRDS("data/data.rds")
pal <- colorNumeric("viridis", NULL)

ui <- fluidPage(
  titlePanel("Mi primer Shiny!"),
  selectInput("region", "Region", choices = unique(data$region)),
  fluidRow(
    column(6, plotlyOutput("plot")),
    column(6, leafletOutput("map"))
    )
  )

server <- function(input, output) {
   
   output$map <- renderLeaflet({
   
     dgeo <- st_read(sprintf("data/shapes/R%s/Comuna.shp", str_pad(input$region, 2, pad = 0)),
                     layer = "Comuna", quiet = TRUE)
     dgeo <- dgeo %>% mutate(COMUNA = as.numeric(as.character(COMUNA)))
     dgeo <- left_join(dgeo, data, by = c("COMUNA" = "CODIGO"))
     
     leaflet(dgeo) %>%
       addTiles() %>%
       addPolygons(
         stroke = TRUE, color = "#DADADA", weight = 1,
         smoothFactor = 1, fillOpacity = 0.75,
         fillColor = ~pal(escolaridad_promedio),
         label = ~paste0(DESC_COMUN, ": ", escolaridad_promedio)
       ) %>%
       addLegend(pal = pal, values = ~escolaridad_promedio, opacity = 1.0)
     
   })
   
   output$plot <- renderPlotly({
     
     p <- data %>% 
       filter(region == input$region) %>% 
       ggplot(aes(escolaridad_promedio, ingreso_promedio_mm,
                  color = escolaridad_promedio, size = personas, label = COMUNA)) +
       geom_point()
     
     config(ggplotly(p), displayModeBar = FALSE)
     
   })
}

shinyApp(ui = ui, server = server)

