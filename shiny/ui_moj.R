library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Test"),
  
  tabsetPanel(
    tabPanel("Test",
             DT::dataTableOutput("tabela.eu.slovenija.preostalaevropa.ostalo.SKUPAJ")),
    
    
  )
))