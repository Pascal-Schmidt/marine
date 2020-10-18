source("helper_fns/info_card.R")

map_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    
    shiny::uiOutput(ns("cards")),
    
    # Map + Table
    div(
      class = "container",
      div(
        column(
          width = 12,
          div(
            class = "panel panel-default",
            div(
              class = "panel-heading text-center",
              tags$h5("Maximum Distance Traveled Between Consecutive Observations \n
                      for a Ship of Selected Name and Type")
            ),
            div(
              class = "panel-body", 
              leaflet::leafletOutput(outputId = ns("map"), height = 400),
              DT::DTOutput(outputId = ns("table"))
            )
          )
        )
      )
    )
    
  )
  
}

map_server <- function(id, df, action_button, df_dt) {
  
  shiny::moduleServer(
    id,
    
    function(input, output, session) {
      
      ns <- shiny::NS(id)
      
      two_points <- shiny::eventReactive(action_button(), {
        
        if(nrow(df()) > 1) {
          
          row <- which(df()[["distance_traveled"]] == max(df()[["distance_traveled"]], na.rm = TRUE))
          
          # always get the most recent observation when ship has sailed same amount of max meters
          # because the df is arranged by datetime, the most recent one will always be the last 
          # instance in the vector "row" that was calculated above
          row <- row[length(row)]
          row <- c(row - 1, row)
          two_points <- df()[row, ] %>% 
            dplyr::mutate(points = c("Start", "Stop"))
          
        } else {
          
          two_points <- df() %>% 
            dplyr::mutate(points = "Start")
          
        }

        return(two_points)
        
      })
      
      output$map <- renderLeaflet({
        
        # if there are ships that 
        if(nrow(two_points()) > 1) {
          
          pal <- leaflet::colorFactor(c("green", "red"), domain = two_points()$points)
          
        } else {

          pal <- leaflet::colorFactor("green", domain = two_points()$points)

        }
        
        leaflet::leaflet(two_points()[, c("lon", "lat")],
                         options = leafletOptions(zoomControl = FALSE)) %>%
          leaflet::addProviderTiles(providers$CartoDB.Positron) %>% 
          leaflet::addCircleMarkers(
            stroke = FALSE, fill = TRUE, fillOpacity = .7,
            color = ~ pal(two_points()$points),
            popup = paste(
            "<strong>Longitude: </strong>", two_points()$lon, "<br/>",
            "<strong>Latitude: </strong>", two_points()$lat, "<br/>",
            "<strong>Ship Type: </strong>", two_points()$ship_type, "<br/>",
            "<strong>Ship Name: </strong>", two_points()$shipname, "<br/>",
            "<strong>Time: </strong>", two_points()$datetime, "<br/>")
          ) %>% 
          addLegend("bottomright",
                    pal = pal,
                    values = ~two_points()$points, title = "Position")
        
        
      })
      
      dt_table_first <- df_dt %>% 
        dplyr::filter(ship_type == "Cargo", shipname == ". PRINCE OF WAVES") %>% 
        .[which(.[["distance_traveled"]] == max(.[["distance_traveled"]], na.rm = TRUE)), ] %>% 
        dplyr::select(shipname, ship_type, distance_traveled)
      
      output$table <- DT::renderDT({
  
        dt_table_first
        
      }, selection = 'none',
      rownames = FALSE, options = list(dom = 't', ordering = F, processing = FALSE))
      
      proxy <- DT::dataTableProxy("table")
      
      shiny::observe({
        
        DT::replaceData(proxy, two_points() %>% 
                          .[nrow(.), ] %>% 
                          dplyr::select(shipname, ship_type, distance_traveled), rownames = FALSE)
        
      })
      
      card_vars <- shiny::eventReactive(two_points(), {
        
        distance <- two_points() %>% 
          .[nrow(.), ] %>% 
          dplyr::pull(distance_traveled) %>% 
          round(2)
        
        total_n <- nrow(df())
        if(nrow(two_points()) > 1) {
          
          time_diff <- base::difftime(two_points()$datetime[length(two_points()$datetime)], 
                                      two_points()$datetime[length(two_points()$datetime) - 1], units = "secs")
          
        } else {
          
          time_diff <- 0
          
        }
        
        return(
          c(
            distance,
            total_n,
            time_diff
          )
        )
        
      })
      
      output$cards <- shiny::renderUI({
        
        div(
          class = "container",
          info_cards(text = stringr::str_glue("Distance Traveled: {card_vars()[1]} meters"), icon = "route"),
          info_cards(text = stringr::str_glue("Total Observations: {card_vars()[2]}"), icon = "hashtag"),
          info_cards(text = stringr::str_glue("Elapsed Time: {card_vars()[3]}"), icon = "clock")
        )
        
      })
    }
  )
  
}