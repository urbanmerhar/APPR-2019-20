# ui za regresija_2
ui <- fluidPage(
  verticalLayout(
    titlePanel("Regresija"),
        tabsetPanel(type = "tabs",
                    tabPanel("Prebivalci s slovenskim državljanstvom", plotOutput("plot1")),
                    tabPanel("Razdeljeno na spola", plotOutput("plot2"))
    )
  )
)