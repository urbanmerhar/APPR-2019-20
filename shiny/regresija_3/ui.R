# ui za regresija_3
ui <- fluidPage(
  verticalLayout(
    titlePanel("Regresija prebivalcev s tujim državljanstvom"),
        tabsetPanel(type = "tabs",
                    tabPanel("Prebivalci s tujim državljanstvom", plotOutput("plot1"),
                             verbatimTextOutput("summary1")),
                    tabPanel("Razdeljeno na spola", plotOutput("plot2"),
                             verbatimTextOutput("summary2"),
                             verbatimTextOutput("summary3"))
    )
  )
)