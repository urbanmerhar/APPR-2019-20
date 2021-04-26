fluidPage(
  verticalLayout(
    titlePanel("Razporeditev prebivalcev brez Slovenskega državljanstva"),
    plotOutput("plot"),
    wellPanel(
      sliderInput("sliderleto",
                  label = "Leto:",
                  min = 2011, max = 2019, value = 2011, step = 1, sep = "")
    )
  )
)