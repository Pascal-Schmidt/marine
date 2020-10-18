library(tidyverse)
library(shiny)
library(DT)
library(plotly)
library(leaflet)
library(feather)
library(geosphere)
library(janitor)
library(shiny.semantic)
library(shinycssloaders)
library(shinyjs)

ships <- feather::read_feather("data/ships.feather")

list.files("modules") %>%
    purrr::map(~ source(paste0("modules/", .)))

ui <- shiny::fluidPage(
    
    shinyjs::useShinyjs(),
    style = "padding-right: 0px;
             padding-left: 0px;",
    
    shiny::navbarPage(
        title = "Port Analytics",
        collapsible = TRUE,
        
        shiny::tabPanel(
            title = "Map",
            icon = shiny::icon("map"),
            
            drop_down_ui("dropdowns", df = ships),
            map_ui("map")
            
            
        ),
        
        shiny::tabPanel(
            title = "Plots",
            icon = shiny::icon("chart-line"),
            
            drop_down_ui("dropdowns_2", df = ships),
            plots_ui("plots")
            
        )
        
    )
    
)

server <- function(input, output, session) {
    
    user_inputs <- drop_down_server(id = "dropdowns", df = ships)
    map_server(id = "map", df = user_inputs$filtered_ship,
               action_button = user_inputs$action_filter, df_dt = ships)
    
    user_inputs_2 <- drop_down_server(id = "dropdowns_2", df = ships)
    plots_server(id = "plots", df = user_inputs_2$filtered_ship,
                 action_button = user_inputs_2$action_filter)
    
}

shinyApp(ui, server)
