# server pie1
function(input, output) {
  output$plot <- renderPlot({
    # y = vsota tujcev
    plot(x = 2011:2019,
         y = (prebivalstvo.real %>%
                group_by(leto, `drzavljanstvo == "Slovenija"`) %>%
                summarise_at(vars(delez), list(stevilo = sum)) %>%
                filter(`drzavljanstvo == "Slovenija"` == FALSE))$stevilo,
         type= "b",
         lwd = "2",
         pch = 16,
         col = 'lightblue',
         xlab= "Leto",
         ylab= "",
         las = 1,
         #main = "Delež prebivalstva s tujim državljanstvom, glede na leto 2011"
    )
  })
}