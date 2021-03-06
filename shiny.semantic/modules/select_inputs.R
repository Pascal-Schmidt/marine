# 1.0 UI ----
drop_down_ui <- function(id, df) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    
    div(
      class = "ui three column stackable grid container",
      
      # 1.1 select ship type ----
      div(
        class = "column",
        shiny.semantic::selectInput(inputId = ns("type"), label = "Select Vessel Type:",
                                    choices = unique(df[["ship_type"]]) %>% 
                                      sort(), width = "100%")
      ),
      
      # 1.2 select ship name ----
      div(
        class = "column",
        shiny.semantic::selectInput(inputId = ns("name"), label = "Select Vessel Name:",
                                    choices = unique(df[["shipname"]]) %>% 
                                      sort(), width = "100%")
      ),
      
      # 1.3 action button filter ----
      div(
        class = "column",
        style = "margin-top:25px;",
        shiny.semantic::actionButton(inputId = ns("filters"), label = "Apply Filters",
                                     icon = shiny::icon("filter"),
                                     width = "100%")
      )
    )
  )
  
}

# 2.0 Server ----
drop_down_server <- function(id, df) {
  
  shiny::moduleServer(
    id,
    
    function(input, output, session) {
      
      # click filter when shiny app starts
      shinyjs::click(id = "filters")
      
      # 2.1 Filter for ship type ----
      ship_names <- shiny::reactive({
        
        df %>% 
          dplyr::filter(ship_type == input$type)
        
      })
      
      # 2.2 Update drop down menu for ship name ----
      shiny::observe({
        shiny.semantic::updateSelectInput(session, inputId = "name",
                                          label = "Select Vessel Name:",
                                          choices = ship_names() %>% 
                                            {unique(.$shipname)} %>% 
                                            sort()) 
      })
      
      # 2.3 Filter for ship name ----
      ship_df <- shiny::reactive({
        
        ship_names() %>% 
          dplyr::filter(shipname == input$name)
        
      })
      
      return(
        list(
          filtered_ship = ship_df,
          action_filter = shiny::reactive(input$filters)
        )
      )
      
    }
  )
  
}