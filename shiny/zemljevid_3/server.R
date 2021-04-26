# server pie1
function(input, output) {
  output$plot <- renderPlot({
    balkan.brez.bih <- balkan[-3]
    balkan.brez.bih.ang <- balkan.ang[-3]
    
    balkan.brez.bih.tabela <- tabela.evropa %>%
                              filter(drzavljanstvo %in% balkan.brez.bih) %>%
                              add_column(rep(balkan.brez.bih.ang, times=9), .after = 'skupaj') %>% 
                              filter(leto == input$sliderletozemljevid)
    
    balkan.brez.bih.zemljevid <- map_data("world", region = balkan.brez.bih.ang)
    
    balkan.brez.bih.tabela.zemljevid <- inner_join(balkan.brez.bih.zemljevid,
                                                   data.frame(region = as.character(balkan.brez.bih.ang), vrednost = as.numeric(balkan.brez.bih.tabela$skupaj)),
                                                   by = "region")
    
    ggplot(data = balkan.brez.bih.zemljevid, mapping = aes(x = long, y = lat, group = group))+
      scale_fill_gradient(name = "Vrednost", low = "lightblue", high = "darkblue", na.value = "grey50") +
      geom_polygon(data = balkan.brez.bih.tabela.zemljevid, aes(fill= vrednost), color = "white") +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      ) +
      theme(legend.position = "right")
  })
}