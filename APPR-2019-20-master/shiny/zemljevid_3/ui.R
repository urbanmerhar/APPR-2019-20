fluidPage(
  verticalLayout(
    titlePanel("Zemljevid Balkana brez BiH"),
    plotOutput("plot"),
    wellPanel(
      sliderInput("sliderletozemljevid",
                  label = "Leto:",
                  min = 2011, max = 2019, value = 2011, step = 1, sep = "")
    )
  )
)