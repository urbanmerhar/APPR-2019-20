# server pie1
function(input, output) {
  output$plot <- renderPlot({
    plot(x = 2011:2019,
         y = prebivalstvo.nom.spola.skupaj.vsi,
         type= "b",
         lwd = "2",
         pch = 16,
         col = 'lightblue',
         xlab= "Leto",
         ylab= "Število prebivalcev",
    )
  })
}