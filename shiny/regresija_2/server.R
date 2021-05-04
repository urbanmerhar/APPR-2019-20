#server regresija_2
server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(df.slovenci, aes(leto, stevilo)) +
      geom_point(color="lightblue", size = 3) +
      geom_abline(intercept = lm.slovenci$coefficients[1], slope = lm.slovenci$coefficients[2], color="black", size=1) +
      theme_bw() +
      scale_x_discrete(limits=c(2011:2019)) + 
      labs(title="",
           x="Leto",
           y="Število prebivalcev")
  })
  
  output$summary1 <- renderPrint({
    summary(lm.slovenci)
  })

  output$plot2 <- renderPlot({
    ggplot(df.slovenci.spol, aes(leto, stevilo)) + 
      geom_point(aes(colour = factor(spol)), size = 3) +
      scale_colour_manual(values=c("lightblue", "lightcoral")) +
      geom_abline(intercept = lm.slovenci.moski$coefficients[1], slope = lm.slovenci.moski$coefficients[2], color="blue", size=1) +
      geom_abline(intercept = lm.slovenci.zenske$coefficients[1], slope = lm.slovenci.zenske$coefficients[2], color="red", size=1) +
      theme_bw() +
      scale_x_discrete(limits=c(2011:2019)) + 
      labs(title="",
           col="Spol",
           x="Leto",
           y="Število prebivalcev")
  })
  
  output$summary2 <- renderPrint({
    summary(lm.slovenci.moski)
  })
  
  output$summary3 <- renderPrint({
    summary(lm.slovenci.zenske)
  })
}
