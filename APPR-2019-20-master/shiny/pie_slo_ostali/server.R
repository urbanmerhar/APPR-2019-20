# server pie1
function(input, output) {
  output$plot <- renderPlot({
    pie((slo.ostali %>% filter(leto == input$sliderleto))$stevilo,
        labels = slo.ostali$drzavljanstvo,
        col = c("darkred", "darkgreen")
        )
  })
}