# server pie1
function(input, output) {
  output$plot <- renderPlot({
    pie((razbitje.ostali %>% filter(leto == input$sliderleto))$skupaj,
        labels = (razbitje.ostali %>% filter(leto == input$sliderleto))$drzavljanstvo)
  })
}