# ui za prikaz surove tabele
library(ggplot2)

fluidPage(
  titlePanel("Surova tabela"),
  
  DT::dataTableOutput("table")
)