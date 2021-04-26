# server pie1
function(input, output) {
  output$plot <- renderPlot({
    drzave.eu <- c("Avstrija", "Belgija", "Bolgarija", "Hrvaška", "Ciper", 'Češka republika', "Danska", "Estonija", "Finska", "Francija",
                   "Nemčija", "Grčija", "Madžarska", "Irska", "Italija", "Latvija", "Litva", "Luksemburg", "Malta", "Nizozemska",
                   "Poljska", "Portugalska", "Romunija", "Slovaška", 'Slovenija', "Španija", "Švedska")
    
    drzave.v.eu <- tabela.evropa %>%
                   filter(drzavljanstvo %in% drzave.eu) %>%
                   add_column(rep(drzave.v.evropski.uniji, times=9), .after = 'skupaj') %>%
                   filter(leto == input$sliderletozemljevid)
    
    drzave.v.eu.zemljevid <- map_data("world", region = drzave.v.evropski.uniji)
    
    drzave.v.eu.tabela.zemljevid <- inner_join(drzave.v.eu.zemljevid,
                                               data.frame(region = as.character(drzave.v.evropski.uniji), vrednost = as.numeric(drzave.v.eu$skupaj)),
                                               by = "region")
    
    ggplot(data = drzave.v.eu.zemljevid %>% filter(region != "Slovenia"), mapping = aes(x = long, y = lat, group = group))+
      geom_polygon(color = "black", fill = NA) +
      geom_polygon(data = drzave.v.eu.tabela.zemljevid %>% filter(region != "Slovenia"), aes(fill= vrednost), color = "white") +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      ) +
      scale_fill_gradient(name = "Vrednost", low = "lightblue", high = "darkblue", na.value = "grey50") +
      theme(legend.position = "right")
  })
}