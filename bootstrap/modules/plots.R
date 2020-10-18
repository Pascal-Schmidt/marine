plots_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    
    div(
      class = "container",
      div(
        column(
          width = 12,
          div(
            class = "panel panel-default",
            div(
              class = "panel-body", 
              plotly::plotlyOutput(outputId = ns("plot"), height = 400),
              DT::DTOutput(outputId = ns("table"))
            )
          )
        )
      )
    )
  )
  
  
  
}

# 2.0 Server ----
plots_server <- function(id, df, action_button) {
  
  shiny::moduleServer(
    id,
    
    function(input, output, session) {
      
      plotly_df <- shiny::eventReactive(action_button(), {
        
        df()
        
      })
      
      output$plot <- plotly::renderPlotly({
        
        plotly_df() %>% 
          .[-1, ] %>% 
          plot_ly(x = ~ datetime) %>%
          add_lines(y = ~fitted(loess(distance_traveled ~ as.numeric(datetime)))) %>%
          add_markers(y = ~distance_traveled) %>%
          layout(showlegend = FALSE,
                 xaxis = list(rangeslider = list(type = "date"),
                              title = "Distance"),
                 yaxis = list(title = ""))
        
      })
      
      output$table <- DT::renderDT({
        
        shiny::isolate(plotly_df())
        
      }, selection = 'none',
      rownames = FALSE, options = list(dom = 't', ordering = F, processing = FALSE))
      
      proxy <- DT::dataTableProxy("table")
      
      shiny::observe({
        
        DT::replaceData(proxy, plotly_df(), rownames = FALSE)
        
      })
      
    }
  )
  
}