# server pie1
function(input, output) {
  output$plot <- renderPlot({
    slo.moski <- prebivalstvo %>%
                 filter(drzavljanstvo == "Slovenija") %>%
                 filter(spol == "Moški") %>%
                 filter(leto == input$sliderletopiramida)
    slo.zenske <- prebivalstvo %>% filter(drzavljanstvo == "Slovenija") %>% filter(spol == "Ženske") %>%
      filter(leto == input$sliderletopiramida)
    sloBAR <- rbind(slo.moski$stevilo, slo.zenske$stevilo)
    #moški_slo
    barplot(100 * sloBAR[1,]/sum(prebivalstvoBAR),
            horiz=TRUE,
            xlim = c(-5, 5),
            col = "lightblue",
            #main = "Porazdelitev prebivalcev po starosti (SLO)",
            xlab = "Število prebivalcev",
            las=1,
            xaxt="n",
            names.arg = prebivalstvo.skupaj$starost[1:19])
    #ženske_slo
    barplot(100 * (-sloBAR[2,]/sum(prebivalstvoBAR)),
            add=TRUE,
            horiz=TRUE,
            col = "lightcoral",
            las=1,
            xaxt="n",
            names.arg = prebivalstvo.skupaj$starost[1:19])
    # ker je xlim nastavljena na [-5,5] se umetno nastavljena axis ujema
    axis(1, at=(-5):5, labels=FALSE, tck=.02)
    mtext(paste0(c(5:1, 0:5), "%"), side = 1, 1, at=(-5):5, las=1)
  })
}