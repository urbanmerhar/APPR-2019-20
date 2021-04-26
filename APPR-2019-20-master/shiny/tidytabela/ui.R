# ui za tidy tabelo
library(ggplot2)

fluidPage(
  titlePanel("Tidy tabela prebivalstva"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(3,
           selectInput("spol",
                       "Spol:",
                       c("Vse",
                         unique(as.character(prebivalstvo$spol))))
    ),
    column(3,
           selectInput("drzavljanstvo",
                       "Državljanstvo:",
                       c("Vsa",
                         unique(as.character(prebivalstvo$drzavljanstvo))))
    ),
    column(3,
           selectInput("leto",
                       "Leto:",
                       c("Vsa",
                         unique(as.integer(prebivalstvo$leto))))
    ),
    column(3,
           selectInput("starost",
                       "Starost:",
                       c("Vse",
                         unique(as.character(prebivalstvo$starost))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)