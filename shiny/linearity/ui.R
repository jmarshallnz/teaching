library(shiny)

shinyUI(fluidPage(
  # Show a plot of the generated distribution
  fluidRow(
    column(width=6, plotOutput("data")),
    column(width=6, plotOutput("resid"))
  ),
  fluidRow(
    column(width=3, sliderInput("curve", "Curvature", 0, 1, 0, step=0.05)),
    column(width=3, sliderInput("hetero", "Non-constant variance", 0, 1, 0, step=0.01)),
    column(width=4, radioButtons("transform", "Transformation", c(None=0, `Log-Log`= 1, Quadratic=2), inline=TRUE)),
    column(width=2, br(actionButton("newdata", "New data set")))
  )
))
