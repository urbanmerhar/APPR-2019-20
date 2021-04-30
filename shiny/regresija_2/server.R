#server regresija_2
server <- function(input, output) {
  output$plot1 <- renderPlot({
    plot(x = df.slovenci$leto, y = df.slovenci$stevilo,
         lwd = "2",
         pch = 16,
         col = 'lightblue',
         xlab= "Leto",
         ylab="Število prebivalcev",
         grid())
    abline(lm.slovenci,
           lwd = "2",
           col = "darkred")
  })

  output$plot2 <- renderPlot({
    plot(x= df.slovenci.spol$leto, y = df.slovenci.spol$stevilo,
         lwd = "2",
         pch = 16,
         col = c('lightblue', "lightcoral"),
         xlab= "Leto",
         ylab= "Število prebivalcev",
         grid())
    abline(lm.slovenci.moski,
           lwd = "2",
           col = "darkblue")
    abline(lm.slovenci.zenske,
           lwd = "2",
           col = "darkred")
  })
}
