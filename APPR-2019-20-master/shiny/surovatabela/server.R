# server za surovo tabelo
library(ggplot2)

function(input, output) {
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- prebivalstvo.surovi.podatki
  }))
  
}