#server regresija_1
server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(df.vsi, aes(leto, stevilo)) +
      geom_point(color="lightblue", size = 3) +
      geom_abline(intercept = lmvsi$coefficients[1], slope = lmvsi$coefficients[2], color="black", size=1) +
      theme_bw() +
      scale_x_discrete(limits=c(2011:2019)) + 
      labs(title="",
           x="Leto",
           y="Število prebivalcev")
  })
  
  output$summary1 <- renderPrint({
    summary(lmvsi)
  })
  
  output$plot2 <- renderPlot({
    ggplot(df.vsi.spol, aes(leto, stevilo)) + 
      geom_point(aes(colour = factor(spol)), size = 3) +
      scale_colour_manual(values=c("lightblue", "lightcoral")) +
      geom_abline(intercept = lm.vsi.moski$coefficients[1], slope = lm.vsi.moski$coefficients[2], color="blue", size=1) +
      geom_abline(intercept = lm.vsi.zenske$coefficients[1], slope = lm.vsi.zenske$coefficients[2], color="red", size=1) +
      theme_bw() +
      scale_x_discrete(limits=c(2011:2019)) + 
      labs(title="",
           col="Spol",
           x="Leto",
           y="Število prebivalcev")
  })
  
  output$summary2 <- renderPrint({
    summary(lm.vsi.moski)
  })
  
  output$summary3 <- renderPrint({
    summary(lm.vsi.zenske)
  })
}
