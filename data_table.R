# library(shiny)
# library(DT)
# 
# dat <- iris[c(1,2,3,51,52,53,101,102,103), c(5,1,2,3,4)]
# 
# ui <- fluidPage(
#   DTOutput("table")
# )
# 
# server <- function(input, output){
#   output[["table"]] <- renderDT({
#     dtable <- datatable(dat, rownames = FALSE, extensions = "Buttons", class = "cell-border stripe",
# 
#                         options = list(dom = "B", buttons = c("pdf"),
#                           rowsGroup = list(0) # merge cells of column 1
#                         ))
#     path <- "yousuf" # folder containing dataTables.rowsGroup.js
#     dep <- htmltools::htmlDependency(
#       "RowsGroup", "2.0.0",
#       path, script = "dataTables.rowsGroup.js")
#     dtable$dependencies <- c(dtable$dependencies, list(dep))
#     dtable
#   })
# }
# 
# shinyApp(ui, server)

library(shiny)
library(flextable)

ui <- fluidPage(

  titlePanel("mtcars"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("mpg", "mpg Limit", min = 11, max = 33, value = 20)
    ),
    mainPanel(
      uiOutput("mtcars_ft")
    )
  )
)

server <- function(input, output) {
  library(dplyr)
  output$mtcars_ft <- renderUI({
    req(input$mpg)
    
    mtcar_tab <- mtcars %>%
      mutate(car = rownames(.)) %>%
      select(car, everything()) %>%
      filter(mpg <= input$mpg) %>%
      flextable() %>%
      theme_box() %>%
      htmltools_value
    
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)