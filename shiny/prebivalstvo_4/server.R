# server pie1
function(input, output) {
  output$plot <- renderPlot({
    plot(x = 2011:2019,
         y = (prebivalstvo.nom.tujci.spola %>% filter(spol == "Moški"))[2:10],
         ylim=range(24000, 91000),
         type= "b",
         lwd = "2",
         pch = 16,
         col = 'lightblue',
         xlab= "Leto",
         ylab= "Število prebivalcev/prebivalk",
         #main= "Število prebivalcev in prebivalk s tujim državljanstvom"
    )
    
    lines(x = 2011:2019,
          y = (prebivalstvo.nom.tujci.spola %>% filter(spol == "Ženske"))[2:10],
          type= "b",
          lwd = "2",
          pch = 16,
          col = 'lightcoral',
          xlab= "Leto",)
    
    legend("topleft",
           title="Spol",
           legend = c("Moški", "Ženske"),
           col = c("lightblue", "lightcoral"),
           lwd = "2",
           cex=0.8)
  })
}