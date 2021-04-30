#server regresija_1
server <- function(input, output) {
  output$plot1 <- renderPlot({
    plot(x = df.vsi$leto, y = df.vsi$stevilo,
         lwd = "2",
         pch = 16,
         col = "lightblue",
         xlab= "Leto",
         ylab= "Število prebivalcev",
         grid())
    abline(lmvsi,
           lwd = "2")
  })
  
  # Generate a summary of the data ----
  output$plot2 <- renderPlot({
    plot(x= df.vsi.spol$leto, y = df.vsi.spol$stevilo,
         lwd = "2",
         pch = 16,
         col = c('lightblue', "lightcoral"),
         xlab= "Leto",
         ylab= "Število prebivalcev",
         grid())
    abline(lm.vsi.moski,
           lwd = "2")
    abline(lm.vsi.zenske,
           lwd = "2")
  })
}
