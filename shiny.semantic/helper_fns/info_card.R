custom_ui_message <- function(head, content, icon_name, size = "big", color = "white") {
  
  div(class = glue::glue("ui icon {size} {color} message"),
      icon(icon_name),
      div(class = "content",
          div(class = "header", head),
          p(content)
      )
  )
  
}