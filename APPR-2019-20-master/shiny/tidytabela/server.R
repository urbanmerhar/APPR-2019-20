# server za tidy tabelo
library(ggplot2)

function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- prebivalstvo
    if (input$spol != "Vse") {
      data <- data[data$spol == input$spol,]
    }
    if (input$drzavljanstvo != "Vsa") {
      data <- data[data$drzavljanstvo == input$drzavljanstvo,]
    }
    if (input$leto != "Vsa") {
      data <- data[data$leto == input$leto,]
    }
    if (input$starost != "Vse") {
      data <- data[data$starost == input$starost,]
    }
    data
  }))
  
}