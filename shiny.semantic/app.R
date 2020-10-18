library(tidyverse)
library(shiny)
library(DT)
library(plotly)
library(leaflet)
library(feather)
library(geosphere)
library(janitor)
library(shiny.semantic)

ships <- feather::read_feather("data/ships.feather")

list.files("modules") %>%
    purrr::map(~ source(paste0("modules/", .)))

ui <- shiny.semantic::semanticPage(
    
    shinyjs::useShinyjs(),
    
    drop_down_ui("dropdowns", df = ships),
    map_ui("map")

)

server <- function(input, output, session) {
    
    user_inputs <- drop_down_server(id = "dropdowns", df = ships)
    map_server(id = "map", df = user_inputs$filtered_ship,
               action_button = user_inputs$action_filter)
    
}

shinyApp(ui, server)
