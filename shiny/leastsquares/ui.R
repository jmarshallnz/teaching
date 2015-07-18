library(shiny)

shinyUI(fluidPage(
  fluidRow(
    column(width=8, plotOutput("plot")),
    column(width=4, wellPanel(
      sliderInput("beta", label="Estimated slope", min=0, max=1, value=0, step=0.01),
      actionButton("fit", "Compute optimal fit")
    ))
  )
))
