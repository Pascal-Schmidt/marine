info_cards <- function(text, icon) {
  
  div(
    column(
      width = 4,
      div(
        class = "panel panel-default",
        div(
          class = "panel-body",
          shiny::icon(icon, class = "pull-right fa-2x"),
          tags$strong(text)
        )
      )
    )
  )
  
}
