fluidPage(
  verticalLayout(
    titlePanel("Zemljevid Evropske Unije brez Hrvaške"),
    plotOutput("plot"),
    wellPanel(
      sliderInput("sliderletozemljevid",
                  label = "Leto:",
                  min = 2011, max = 2019, value = 2011, step = 1, sep = "")
    )
  )
)