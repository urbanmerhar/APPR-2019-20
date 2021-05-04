# server pie1
function(input, output) {
  output$plot <- renderPlot({
    # y = vsota vseh prebivalcev, ločeni po spolu
    plot(x = 2011:2019,
         y = (prebivalstvo.real %>%
                group_by(spol, leto) %>%
                summarise_at(vars(delez), list(stevilo = sum)) %>%
                filter(spol == "Moški"))$stevilo,
         ylim=range(0.49, 0.51),
         type= "b",
         lwd = "2",
         pch = 16,
         col = 'lightblue',
         xlab= "Leto",
         ylab= "",
         las = 1,
         #main = "Delež prebivalstva ločen po spolu"
    )
    
    lines(x = 2011:2019,
          y = (prebivalstvo.real %>%
                 group_by(spol, leto) %>%
                 summarise_at(vars(delez), list(stevilo = sum)) %>%
                 filter(spol == "Ženske"))$stevilo,
          type= "b",
          lwd = "2",
          pch = 16,
          col = 'lightcoral'
    )
    
    legend("bottomright",
           title="Spol",
           legend = c("Moški", "Ženske"),
           col = c("lightblue", "lightcoral"),
           lwd = "2",
           cex=0.8)
  })
}