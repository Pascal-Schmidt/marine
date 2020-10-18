# load cards function
source("helper_fns/info_card.R")

# 1.0 UI ----
map_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    
    # 1.1 info cards ----
    shiny::uiOutput(ns("cards")),
    
    br(),
    
    # 1.2 Map + Table ----
    div(class = "ui container",
        div(
          class = "ui raised centered segment",
          leaflet::leafletOutput(outputId = ns("map"), height = 400),
          DT::DTOutput(outputId = ns("table"))
        )
    )
  )
        
  
}

map_server <- function(id, df, action_button) {
  
  shiny::moduleServer(
    id,
    
    function(input, output, session) {
      
      ns <- shiny::NS(id)
      
      # when action button is clicked, get max distance
      two_points <- shiny::eventReactive(action_button(), {
        
        if(nrow(df()) > 1) {
          
          # get back rows that have max distance
          row <- which(df()[["distance_traveled"]] == max(df()[["distance_traveled"]], na.rm = TRUE))
          
          # always get the most recent observation when ship has sailed same amount of max meters
          # because the df is arranged by datetime, the most recent one will always be the last 
          # instance in the vector "row" that was calculated above
          row <- row[length(row)]
          row <- c(row - 1, row)
          two_points <- df()[row, ] %>% 
            dplyr::mutate(points = c("Start", "Stop"))
          
        } else {
          
          # there are 3 names, for which there is only one observation
          two_points <- df() %>% 
            dplyr::mutate(points = "Start")
          
        }

        return(two_points)
        
      })
      
      # render map
      output$map <- renderLeaflet({
        
        # for ships where there is more than one observation 
        if(nrow(two_points()) > 1) {
          
          pal <- leaflet::colorFactor(c("green", "red"), domain = two_points()$points)
          
          # for ships where there is only one observation
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
      
      # at the beginning of the app, get ship name and type and show data table
      # afterwards, use proxy to replace data instead of rendering entire table
      dt_table_first <- ships %>% 
        dplyr::filter(ship_type == "Cargo", shipname == ". PRINCE OF WAVES") %>% 
        .[which(.[["distance_traveled"]] == max(.[["distance_traveled"]], na.rm = TRUE)), ] %>% 
        dplyr::select(shipname, ship_type, distance_traveled)
      
      # render data table
      output$table <- DT::renderDT({
        
        dt_table_first
        
      }, selection = 'none',
      rownames = FALSE, options = list(dom = 't', ordering = F, processing = FALSE))
      
      # use data table proxy 
      proxy <- DT::dataTableProxy("table")
      
      shiny::observe({
        
        DT::replaceData(proxy, two_points() %>% 
                          .[nrow(.), ] %>% 
                          dplyr::select(shipname, ship_type, distance_traveled), rownames = FALSE)
        
      })
      
      # claculate card values every time two_points data frame changes
      card_vars <- shiny::eventReactive(two_points(), {
        
        distance <- two_points() %>% 
          .[nrow(.), ] %>% 
          dplyr::pull(distance_traveled)
        
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
      
      # create cards 
      output$cards <- shiny::renderUI({
        
        div(
          class = "ui three column stackable grid container",
          div(class = "column",
              custom_ui_message(head = card_vars()[1], 
                                icon_name = "route",
                                content = "Meters Traveled")
          ),
          div(
            class = "column",
            custom_ui_message(head = card_vars()[2], 
                              icon_name = "hashtag",
                              content = "Total Observations")
          ),
          div(
            class = "column",
            custom_ui_message(head = card_vars()[3], 
                              icon_name = "clock",
                              content = "Seconds Elapsed")
          )
        )
        
      })
      
    }
  )
  
}