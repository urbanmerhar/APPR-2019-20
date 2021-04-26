# server pie1
function(input, output) {
  output$plot <- renderPlot({
    pie((tabela.eu.slovenija.preostalaevropa.ostalo %>% filter(leto == input$sliderleto))$stevilo,
        labels = tabela.eu.slovenija.preostalaevropa.ostalo$drzavljanstvo,
        col = c("black", "darkred", "darkblue", "darkgreen")
      )
  })
}