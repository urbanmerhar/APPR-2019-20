# ui za regresija_2
ui <- fluidPage(
  verticalLayout(
    titlePanel("Regresija prebivalcev s slovenskim državljanstvom"),
        tabsetPanel(type = "tabs",
                    tabPanel("Prebivalci s slovenskim državljanstvom", plotOutput("plot1"),
                             verbatimTextOutput("summary1")),
                    tabPanel("Razdeljeno na spola", plotOutput("plot2"),
                             verbatimTextOutput("summary2"),
                             verbatimTextOutput("summary3"))
    )
  )
)