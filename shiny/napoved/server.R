# server napoved
function(input, output) {
  output$plot <- renderPlot({
    ggplot() + xlim(2011, 2700) +
      geom_abline(intercept = lmvsi$coefficients[1], slope = lmvsi$coefficients[2], color="black", size=1) +
      geom_abline(intercept = lm.slovenci$coefficients[1], slope = lm.slovenci$coefficients[2], color="darkgreen", size=1) +
      geom_abline(intercept = lm.tujci$coefficients[1], slope = lm.tujci$coefficients[2], color="darkred", size=1) +
      scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","), limits= c(0, 4000000)) +
      theme_bw() +
      labs(x="Leto",
           y="Število prebivalcev")
  })
}