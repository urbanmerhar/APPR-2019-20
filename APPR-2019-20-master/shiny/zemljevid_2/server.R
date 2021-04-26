# server pie1
function(input, output) {
  output$plot <- renderPlot({
    balkan <- c("Albanija", "Bolgarija", "Bosna in Hercegovina", "Črna gora", "Grčija", "Hrvaška", "Kosovo", "Severna Makedonija",
                "Srbija")
    
    balkan.ang <- c("Albania", "Bulgaria", "Bosnia and Herzegovina", "Montenegro","Greece", "Croatia", "Kosovo", "Macedonia", "Serbia")
    
    balkan.tabela <- tabela.evropa %>% 
                     filter(drzavljanstvo %in% balkan) %>%
                     add_column(rep(balkan.ang, times=9), .after = 'skupaj') %>%
                     filter(leto == input$sliderletozemljevid)
    
    balkanske.drzave.zemljevid <- map_data("world", region = balkan.ang)
    
    balkan.tabela.zemljevid <- inner_join(balkanske.drzave.zemljevid, 
                                          data.frame(region = as.character(balkan.ang), vrednost = as.numeric(balkan.tabela$skupaj)), by= "region")
    
    ggplot(data = balkanske.drzave.zemljevid, mapping = aes(x = long, y = lat, group = group)) +
      scale_fill_gradient(name = "Vrednost", low = "lightblue", high = "darkblue", na.value = "grey50") +
      geom_polygon(data = balkan.tabela.zemljevid, aes(fill= vrednost), color = "white") +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      ) + 
      theme(legend.position = "right")
  })
}