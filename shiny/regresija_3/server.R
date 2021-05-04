#server regresija_3
server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(df.tujci, aes(leto, stevilo)) +
      geom_point(color="lightblue", size = 3) +
      geom_abline(intercept = lm.tujci$coefficients[1], slope = lm.tujci$coefficients[2], color="black", size=1) +
      theme_bw() +
      scale_x_discrete(limits=c(2011:2019)) + 
      labs(title="",
           x="Leto",
           y="Število prebivalcev")
  })
  
  output$summary1 <- renderPrint({
    summary(lm.tujci)
  })

  output$plot2 <- renderPlot({
    ggplot(df.tujci.spol, aes(leto, stevilo)) + 
      geom_point(aes(colour = factor(spol)), size = 3) +
      scale_colour_manual(values=c("lightblue", "lightcoral")) +
      geom_abline(intercept = lm.tujci.moski$coefficients[1], slope = lm.tujci.moski$coefficients[2], color="blue", size=1) +
      geom_abline(intercept = lm.tujci.zenske$coefficients[1], slope = lm.tujci.zenske$coefficients[2], color="red", size=1) +
      theme_bw() +
      scale_x_discrete(limits=c(2011:2019)) + 
      labs(title="",
           col="Spol",
           x="Leto",
           y="Število prebivalcev")
  })
  
  output$summary2 <- renderPrint({
    summary(lm.tujci.moski)
  })
  
  output$summary3 <- renderPrint({
    summary(lm.tujci.zenske)
  })
}
