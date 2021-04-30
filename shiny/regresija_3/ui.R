# ui za regresija_3
ui <- fluidPage(
  verticalLayout(
    titlePanel("Regresija"),
        tabsetPanel(type = "tabs",
                    tabPanel("Prebivalci s tujim državljanstvom", plotOutput("plot1")),
                    tabPanel("Razdeljeno na spola", plotOutput("plot2"))
    )
  )
)