# server pie1
function(input, output) {
  output$plot <- renderPlot({
    tujci.skupaj <- prebivalstvo %>%
                    filter(drzavljanstvo != "Slovenija") %>%
                    group_by(spol, leto , starost) %>%
                    summarise_at(vars(stevilo), list(stevilo = sum)) %>%
                    slice(match(c("0-4 let", "5-9 let", "10-14 let", "15-19 let", "20-24 let", "25-29 let", "30-34 let", 
                                  "35-39 let", "40-44 let", "45-49 let", "50-54 let", "55-59 let", "60-64 let", "65-69 let",
                                  "70-74 let", "75-79 let", "80-84 let", "85-89 let", "90 + let"), starost)) %>%
                    filter(leto == input$sliderletopiramida)
    
    tujciBAR <- rbind((tujci.skupaj %>% filter(spol == "Moški"))$stevilo,
                      (tujci.skupaj %>% filter(spol == "Ženske"))$stevilo)
    
    #moški_tujci
    barplot(100 * tujciBAR[1,]/sum(prebivalstvoBAR),
            horiz=TRUE,
            xlim = c(-0.6, 0.6),
            col = "lightblue",
            #main = "Porazdelitev prebivalcev po starosti (TUJCI)",
            xlab = "Število prebivalcev",
            las=1,
            xaxt="n",
            names.arg = prebivalstvo.skupaj$starost[1:19])
    #ženske_tujci
    barplot(100 * (-tujciBAR[2,]/sum(prebivalstvoBAR)),
            add=TRUE,
            horiz=TRUE,
            col = "lightcoral",
            las=1,
            xaxt="n",
            names.arg = prebivalstvo.skupaj$starost[1:19])
    # ker je xlim nastavljena na [-5,5] se umetno nastavljena axis ujema
    axis(side = 1, at=((-6):6)/10, labels=FALSE, tck=.02)
    mtext(paste0(c(6:1, 0:6)/10, "%"), side = 1, line = 1, at=((-6):6)/10, las=1)
  })
}