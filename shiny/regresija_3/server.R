#server regresija_3
server <- function(input, output) {
  output$plot1 <- renderPlot({
    plot(x = df.tujci$leto, y = df.tujci$stevilo,
         lwd = "2",
         pch = 16,
         col = 'lightblue',
         xlab= "Leto",
         ylab= "Število prebivalcev",
         grid())
    abline(lm.tujci,
           lwd = "2",
           col = "darkred")
  })

  output$plot2 <- renderPlot({
    plot(x= df.tujci.spol$leto, y = df.tujci.spol$stevilo,
         lwd = "2",
         pch = 16,
         col = c('lightblue', "lightcoral"),
         xlab= "Leto",
         ylab="Število prebivalcev",
         grid())
    abline(lm.tujci.moski,
           lwd = "2",
           col = "darkblue")
    abline(lm.tujci.zenske,
           lwd = "2",
           col = "darkred")
  })
}
