# server pie1
function(input, output) {
  output$plot <- renderPlot({
    plot(x = 2011:2019,
         y = (prebivalstvo.nom.spola.skupaj %>% filter(drzavljanstvo == "Slovenija"))[1,][c(2:10)],
         ylim=range(1940000, 2100000),
         type= "b",
         lwd = "2",
         pch = 16,
         col = 'lightblue',
         xlab= "Leto",
         ylab= "Število prebivalcev",
         #main= "Število državljanov kombinacija"
    )
    
    lines(x = 2011:2019,
          y = prebivalstvo.nom.spola.skupaj.vsi,
          type= "b",
          lwd = "2",
          pch = 16,
          col = 'red',
          xlab= "Leto",)
    
    legend("topleft",
           title="Državljanstvo",
           legend = c("Slovensko", "Skupaj"),
           col = c("lightblue", "red"),
           lwd = "2",
           cex = 0.8)
  })
}