
## Load and install the packages
library("tidyverse", "shiny", "stringr")
theme_set(theme_minimal())


# Define server logic
function(input, output, session) {
  observeEvent(input$vHH, {
    vHH = input$vHH
    updateSliderInput(session, "vHL", min = 2, max = vHH -2)
  })
  
  observeEvent(input$vHL, {
    vHL = input$vHL
    updateSliderInput(session, "vLL", min = 1, max = vHL -1)
  })
  
  observeEvent(c(input$vHL, input$vHH, input$vLL), {
    vHL = input$vHL
    vHH = input$vHH
    vLL = input$vLL
    updateSliderInput(session, "vLH", min = 1, max = vHH+vLL-vHL-1)
  })
  
}
  