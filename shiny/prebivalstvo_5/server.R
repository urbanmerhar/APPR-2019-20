# server pie1
function(input, output) {
  output$plot <- renderPlot({
    # y = vsota vseh prebivalcev
    plot(x = 2011:2019,
         y = (prebivalstvo.real %>%
                group_by(leto) %>%
                summarise_at(vars(delez), list(stevilo = sum)))$stevilo,
         type= "b",
         lwd = "2",
         pch = 16,
         col = 'lightblue',
         xlab= "Leto",
         ylab= "",
         las = 1,
         #main = "Delež prebivalstva glede na leto 2011"
    )
  })
}