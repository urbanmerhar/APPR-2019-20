# server pie1
function(input, output) {
  output$plot <- renderPlot({
    testZ <- tabela.evropa %>% 
             add_column(rep(evropske.drzave.ang, times=9), .after = 'skupaj') %>%
             filter(leto == input$sliderletozemljevid)
    test <- testZ$skupaj
    # v grafih za poročilo, se zamenja vektor pri "vrednost"
    test.tabela <- data.frame(region = as.character(evropske.drzave.ang),
                              vrednost = as.numeric(test))
    
    test.tabela.zemljevid <- inner_join(evropske.drzave.zemljevid.ang, test.tabela, by = "region")
    
    ggplot(data = evropske.drzave.zemljevid.ang %>% filter(region != "Slovenia"), mapping = aes(x = long, y = lat, group = group)) +
      scale_fill_gradient(name = "Vrednost", low = "lightblue", high = "darkblue", na.value = "grey50") +
      geom_polygon(data = test.tabela.zemljevid %>% filter(region != "Slovenia"), aes(fill= vrednost), color = "white") +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      )
  })
}