# server pie1
function(input, output) {
  output$plot <- renderPlot({
    ggplot(tabela.eu.slovenija.preostalaevropa.ostalo, aes(fill=drzavljanstvo, y=stevilo, x=leto)) + 
      geom_bar(position="fill", stat="identity") +
      xlab("Leto") +
      scale_x_continuous(breaks=seq(2011, 2019, 1)) +
      ylab("Delež") +
      scale_fill_manual("Državljanstvo", values = c("Slovensko" = "darkgreen", "Evropsko" = "darkblue", "Evropa brez EU" = "darkred", "Ostalo" = "black"))
    
  })
}