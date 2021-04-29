# ui za regresija_1
ui <- fluidPage(
  verticalLayout(
    titlePanel("Regresija"),
        tabsetPanel(type = "tabs",
                    tabPanel("Vsi prebivalci", plotOutput("plot1")),
                    tabPanel("Razdeljeno na spola", plotOutput("plot2"))
    )
  )
)