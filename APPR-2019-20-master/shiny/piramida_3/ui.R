fluidPage(
  verticalLayout(
    titlePanel("Porazdelitev prebivalcev po starosti (TUJCI)"),
    plotOutput("plot"),
    wellPanel(
      sliderInput("sliderletopiramida", label = "Leto:",
                   min = 2011, max = 2019, value = 2011, step = 1, sep = "")
    )
  )
)