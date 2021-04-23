# Server za shiny
library(shiny)

shinyServer(function(input, output){
  output$tabela.eu.slovenija.preostalaevropa.ostalo.SKUPAJ <- DT::renderDataTable({
    tabela.eu.slovenija.preostalaevropa.ostalo.SKUPAJ %>% spread(key="drzavljanstvo", value="stevilo") %>%
      rename(`Število`=stevilo)
  })
  
})

