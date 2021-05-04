# ui za regresija_1
ui <- fluidPage(
  verticalLayout(
    titlePanel("Regresija prebivalcev"),
        tabsetPanel(type = "tabs",
                    tabPanel("Vsi prebivalci", plotOutput("plot1"),
                             verbatimTextOutput("summary1")),
                    tabPanel("Razdeljeno na spola", plotOutput("plot2"),
                             verbatimTextOutput("summary2"),
                             verbatimTextOutput("summary3"))
    )
  )
)