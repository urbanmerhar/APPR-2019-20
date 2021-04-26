# server pie1
function(input, output) {
  output$plot <- renderPlot({
    prebivalstvo.skupaj <- prebivalstvo %>% 
                           group_by(spol, leto, starost) %>%
                           summarise_at(vars(stevilo), list(stevilo = sum)) %>%
                           slice(match(c("0-4 let", "5-9 let", "10-14 let", "15-19 let", "20-24 let", "25-29 let", "30-34 let", 
                                         "35-39 let", "40-44 let", "45-49 let", "50-54 let", "55-59 let", "60-64 let", "65-69 let",
                                         "70-74 let", "75-79 let", "80-84 let", "85-89 let", "90 + let"), starost)) %>%
                           filter(leto == input$sliderletopiramida)
    
    prebivalstvoBAR <- rbind((prebivalstvo.skupaj %>% filter(spol == "Moški"))$stevilo,
                             (prebivalstvo.skupaj %>% filter(spol == "Ženske"))$stevilo)
    #moški
    barplot(100 * prebivalstvoBAR[1,]/sum(prebivalstvoBAR),
            horiz=TRUE,
            xlim = c(-5, 5),
            col = "lightblue",
            #main = "Porazdelitev prebivalcev po starosti (VSI)",
            xlab = "Število prebivalcev",
            las=1,
            xaxt="n",
            names.arg = prebivalstvo.skupaj$starost[1:19])
    #ženske
    barplot(100 * (-prebivalstvoBAR[2,]/sum(prebivalstvoBAR)),
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