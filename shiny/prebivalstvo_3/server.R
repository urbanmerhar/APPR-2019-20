# server pie1
function(input, output) {
  output$plot <- renderPlot({
    plot(x = 2011:2019,
         y = (prebivalstvo.nom %>% filter(spol == "Moški", drzavljanstvo == "Slovenija"))[1,][c(3:11)],
         ylim=range(940000, 1020000),
         type= "b",
         lwd = "2",
         pch = 16,
         col = 'lightblue',
         xlab= "Leto",
         ylab= "Število prebivalcev/prebivalk",
         #main= "Število prebivalcev in prebivalk s slovenskim državljanstvom"
    )
    
    lines(x = 2011:2019,
          y = (prebivalstvo.nom %>% filter(spol == "Ženske", drzavljanstvo == "Slovenija"))[1,][c(3:11)],
          type= "b",
          lwd = "2",
          pch = 16,
          col = 'lightcoral'
    )
    
    
    legend("left",
           title="Spol",
           legend = c("Moški", "Ženske"),
           col = c("lightblue", "lightcoral"),
           lwd = "2",
           cex=0.8)
  })
}